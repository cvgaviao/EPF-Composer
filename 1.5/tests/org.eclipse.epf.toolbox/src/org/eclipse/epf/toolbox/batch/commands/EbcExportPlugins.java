package org.eclipse.epf.toolbox.batch.commands;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.epf.export.services.PluginExportData;
import org.eclipse.epf.export.services.PluginExportService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class EbcExportPlugins extends EpfBatchCommandImpl {

	private List<MethodPlugin> selectedPlugins;

	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.ExportAll, element.getAttribute(C.ExportAll));
		setAttribute(C.ExportFolderPath, element.getAttribute(C.ExportFolderPath));
	}	
	
	public void parseAtExecute() {		
		Element element = getElement();
		
		MethodLibrary lib = getMgr().getCurrentBaseLib();
		boolean exportAll = parseBoolean(C.ExportAll, false);
		if (exportAll) {
			selectedPlugins = new ArrayList<MethodPlugin>(lib.getMethodPlugins());
		} else {
			Map<String, MethodPlugin> nameMap = new HashMap<String, MethodPlugin>();
			for (MethodPlugin plugin : (List<MethodPlugin>) lib.getMethodPlugins()) {
				nameMap.put(plugin.getName(), plugin);
			}
			
			selectedPlugins = new ArrayList<MethodPlugin>();
			NodeList nodes = element.getChildNodes();
			int sz = nodes == null ? 0 : nodes.getLength();
			if (sz > 0) {
				for (int i=0; i<sz; i++) {
					Node node = nodes.item(i);
					if (node instanceof Element) {
						Element cElement = (Element) node;
						if (cElement.getTagName().equals(C.SelectedPlugin)) {
							String name  = cElement.getAttribute(C.Name);
							MethodPlugin plugin = nameMap.get(name);
							if (plugin != null) {
								selectedPlugins.add(plugin);
							}
						}
					}
				}
			}
		}
		
		
	}

	protected void executeBody(final EbcExeReply reply) {
		MethodPlugin plugin = (MethodPlugin) selectedPlugins.get(0);
		File file = new File(plugin.eResource().getURI().toFileString());
		file = file.getParentFile().getParentFile();
		
		String path = getAttribute(C.ExportFolderPath);
		String exportFolderPath = getMgr().getRootPath() + File.separator + path;
		File exportFolder = new File(exportFolderPath);
		
		final PluginExportData data = new PluginExportData();
		data.llData.setLibName(file.getName());
		data.llData.setParentFolder(exportFolder.getAbsolutePath());
		data.setSelectedPlugins(selectedPlugins);
		data.buildAssociatedConfigMap();		

		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				try {
					(new PluginExportService(data)).run(new NullProgressMonitor());
				} catch (Exception e) {
					reply.addStatus(Status.ERROR, e.getMessage(), e);
				}
			}
		});
	}
	
}
