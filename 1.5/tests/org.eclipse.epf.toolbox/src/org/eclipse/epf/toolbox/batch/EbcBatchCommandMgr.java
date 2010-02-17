package org.eclipse.epf.toolbox.batch;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.xmi.XMILibraryUtil;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.batch.commands.EbcExportConfiguration;
import org.eclipse.epf.toolbox.batch.commands.EbcExportPlugins;
import org.eclipse.epf.toolbox.batch.commands.EbcExportXml;
import org.eclipse.epf.toolbox.batch.commands.EbcImportConfiguration;
import org.eclipse.epf.toolbox.batch.commands.EbcImportPlugins;
import org.eclipse.epf.toolbox.batch.commands.EbcImportXml;
import org.eclipse.epf.toolbox.batch.commands.EbcOpenLibrary;
import org.eclipse.epf.toolbox.batch.commands.EbcReportMethodElement;
import org.eclipse.epf.toolbox.batch.commands.EpfBatchCommand;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Weiping Lu
 */
public class EbcBatchCommandMgr {			
		
	private boolean trace = false;	
	private PrintStream tracePS;
	private String rootPath;
	private File ebcInputFile;
	private EbcLibraryService service = EbcLibraryService.getInstance();
	private MethodLibrary currentBaseLib;

	private EbcReportMgr reportMgr;
	
	private List<EpfBatchCommand> epfBatchCommandList = new ArrayList<EpfBatchCommand>();
	
	private static Map<String, EpfBatchCommand> ebcClassMap = new HashMap<String, EpfBatchCommand>();
	private static Map<Class, String> ebcTagNameMap = new HashMap<Class, String>();
	
	static {
		addToEbcClassMap(C.OpenLibrary, EbcOpenLibrary.class);
		addToEbcClassMap(C.ExportPlugins, EbcExportPlugins.class);
		addToEbcClassMap(C.ImportPlugins, EbcImportPlugins.class);		
		addToEbcClassMap(C.ExportConfiguration, EbcExportConfiguration.class);
		addToEbcClassMap(C.ImportConfiguration, EbcImportConfiguration.class);
		addToEbcClassMap(C.ExportXml, EbcExportXml.class);
		addToEbcClassMap(C.ImportXml, EbcImportXml.class);
		addToEbcClassMap(C.ReportMethodElement, EbcReportMethodElement.class);
		
	}
	
	private static void addToEbcClassMap(String elementName, Class cls) {
		Object instance = null;
		try {
			instance = cls.newInstance();
		} catch (Exception e){
			ToolboxPlugin.getDefault().getLogger().logError(e);
		}
		if (instance instanceof EpfBatchCommand) {
			ebcClassMap.put(elementName, (EpfBatchCommand) instance);
			ebcTagNameMap.put(cls, elementName);
		}		
	}

	public EbcBatchCommandMgr(File inputFile) {
		ebcInputFile = inputFile;
		reportMgr = new EbcReportMgr(this);
	}	
	
	public String getTagName(EpfBatchCommand epfInstance) {
		return ebcTagNameMap.get(epfInstance.getClass());
	}			
	
	public EbcExeReplies execute() {
		ToolboxPlugin.getDefault().getLogger().logInfo("Begin executing: " + ebcInputFile); //$NON-NLS-1$

		EbcExeReplies result = null;
		boolean loaded = false;
		Exception loadException = null;
		try {
			loaded = loadEbcInputFile();			
		} catch (Exception e) {
			ToolboxPlugin.getDefault().getLogger().logError(e);
		}
		
		if (loaded) {			
			result = executeCommands();		
		} else {
		}
		
		getReportMgr().saveDocument();
		
		ToolboxPlugin.getDefault().getLogger().logInfo("End executing: " + ebcInputFile); //$NON-NLS-1$
		return result;
	}
	
	private boolean loadEbcInputFile() throws Exception {
		Document document = XMLUtil.loadXml(ebcInputFile);	
		Element root = document.getDocumentElement();

		trace = root.getAttribute(C.Trace).equalsIgnoreCase("true");
		rootPath = root.getAttribute(C.RootPath);
		
		String reportFilePath = root.getAttribute(C.ReportFilePath);
		if (reportFilePath.length() > 0) {
			getReportMgr().setReportFile(new File(rootPath + File.separator + reportFilePath));
		}
		
		if (trace) {
			String traceFilePath = root.getAttribute(C.TraceFilePath);
			File traceFile = new File(rootPath, traceFilePath);
			try {
				tracePS = new PrintStream(new FileOutputStream(traceFile));
			} catch (Exception e){
				ToolboxPlugin.getDefault().getLogger().logError(e);
				trace = false;
			}
		}

		trace("Begin parsing: " + ebcInputFile);
		
		epfBatchCommandList.clear();
		NodeList nodes = root.getChildNodes();
		int sz = nodes == null ? 0 : nodes.getLength();
		for (int i = 0; i < sz; i++) {
			Node node = nodes.item(i);
			if (node instanceof Element) {
				Element element = (Element) node;

				if (element.getTagName().equalsIgnoreCase(C.End)) {
					break;
				}
				addCommand((Element) node);
			}
		}
		
		trace("End parsing: " + ebcInputFile + "\n");
		return true;
	}
	
	public EbcExeReplies executeCommands() {
		EbcExeReplies result = new EbcExeReplies();
		for (int i = 0; i < epfBatchCommandList.size(); i++) {
			EpfBatchCommand command = (EpfBatchCommand) epfBatchCommandList.get(i);
			EbcExeReply reply = null;
			try {
				reply = command.execute();
			} catch (Exception e) {
				e.printStackTrace();
				reply = new EbcExeReply();
			}
			if (reply != null) {
				result.add(reply);
			}
		}			
		
		return result;
	}
	
	public void trace(String line) {
		if (tracePS != null) {
			tracePS.println(line);
		}		
	}
	
	private void addCommand(Element element) {
		if (! element.getAttribute(C.Disable).equals("true")) {
			EpfBatchCommand command = newEpfBatchCommand(element.getTagName());
			if (command != null) {
				command.parse(element);
				epfBatchCommandList.add(command);
			}
		}
	}	
	
	private EpfBatchCommand newEpfBatchCommand(String comandName) {
		EpfBatchCommand cm = ebcClassMap.get(comandName);
		return cm == null ? null : newEpfBatchCommand(cm.getClass());
	}
	
	public EpfBatchCommand newEpfBatchCommand(Class cls) {
		EpfBatchCommand ret = null;
		try {
			ret = (EpfBatchCommand) cls.newInstance();
			ret.setMgr(this);
		} catch (Exception e) {
			ToolboxPlugin.getDefault().getLogger().logError(e);
		}
		return ret;
	}	
	
	public MethodLibrary loadBaseLib(String path) {
		if (currentBaseLib != null) {
			service.closeLibrary(currentBaseLib);
		}
		String libPath = rootPath + File.separator + path;	
		try {
			currentBaseLib = XMILibraryUtil.openMethodLibrary(libPath);
		} catch (Exception e) {
			ToolboxPlugin.getDefault().getLogger().logError(e);
			currentBaseLib = null;
		}
		return currentBaseLib;
	}

	public boolean isTrace() {
		return trace;
	}

	public MethodLibrary getCurrentBaseLib() {
		return currentBaseLib;
	}

	public String getRootPath() {
		return rootPath;
	}			

	public EbcReportMgr getReportMgr() {
		return reportMgr;
	}

	public EbcLibraryService getService() {
		return service;
	}

	public void setCurrentBaseLib(MethodLibrary currentBaseLib) {
		this.currentBaseLib = currentBaseLib;
	}
	
}
