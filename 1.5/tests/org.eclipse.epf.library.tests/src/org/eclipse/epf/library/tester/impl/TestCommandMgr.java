//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.tester.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.importing.services.PluginImportingService;
import org.eclipse.epf.library.project.MethodLibraryProject;
import org.eclipse.epf.library.tester.LibraryDiffAnalyzor;
import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.library.tester.iface.LibraryJunitTest;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.iface.TCExeReplyList;
import org.eclipse.epf.library.tester.iface.TestCommand;
import org.eclipse.epf.library.tester.iface.TestTracer;
import org.eclipse.epf.library.tester.impl.testcommands.TCCircularDependencyCheck;
import org.eclipse.epf.library.tester.impl.testcommands.TCCompareToGoldenFile;
import org.eclipse.epf.library.tester.impl.testcommands.TCCompareToLibrary;
import org.eclipse.epf.library.tester.impl.testcommands.TCCopyLibrary;
import org.eclipse.epf.library.tester.impl.testcommands.TCEditMethodElement;
import org.eclipse.epf.library.tester.impl.testcommands.TCExeReplyImpl;
import org.eclipse.epf.library.tester.impl.testcommands.TCExeReplyListImpl;
import org.eclipse.epf.library.tester.impl.testcommands.TCExportConfiguration;
import org.eclipse.epf.library.tester.impl.testcommands.TCExportPlugins;
import org.eclipse.epf.library.tester.impl.testcommands.TCExportXml;
import org.eclipse.epf.library.tester.impl.testcommands.TCImportConfiguration;
import org.eclipse.epf.library.tester.impl.testcommands.TCImportPlugins;
import org.eclipse.epf.library.tester.impl.testcommands.TCImportXml;
import org.eclipse.epf.library.tester.impl.testcommands.TCNewMethodConfiguration;
import org.eclipse.epf.library.tester.impl.testcommands.TCNewMethodElement;
import org.eclipse.epf.library.tester.impl.testcommands.TCNewMethodPlugin;
import org.eclipse.epf.library.tester.impl.testcommands.TCOpenLibrary;
import org.eclipse.epf.library.tester.impl.testcommands.TCOutputMethodElement;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.library.xmi.XMILibraryUtil;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Used in JUnit tests 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TestCommandMgr implements TestTracer {	
	
	//All the tags will be formally specified and read from
	//a schema later.	
	private static String TCCopyLibrary = "CopyLibrary";
	private static String TCOpenLibrary = "OpenLibrary";
	private static String TCExportPlugins = "ExportPlugins";
	private static String TCImportPlugins = "ImportPlugins";	
	private static String TCExportConfiguration = "ExportConfiguration";
	private static String TCImportConfiguration = "ImportConfiguration";
	private static String TCExportXml = "ExportXml";
	private static String TCImportXml = "ImportXml";
	private static String TCCompareToLibrary = "CompareToLibrary";
	private static String TCCompareToGoldenFile = "CompareToGoldenFile";
	private static String TCNewMethodPlugin = "NewMethodPlugin";
	private static String TCNewMethodConfiguration = "NewMethodConfiguration";
	private static String TCNewMethodElement = "NewMethodElement";
	private static String TCEditMethodElement = "EditMethodElement";
	private static String TCCircularDependencyCheck = "CircularDependencyCheck";
	private static String TCEnd = "End";
	
	private static String TCEditPlugin="EditPlugin";
	private static String TCEditConfiguration="EditConfiguration";
	private static String TCEditContentPackage="EditContentPackage";
	
	public static String TCOutputLibrary="OutputLibrary";
	public static String TCOutputPlugin="OutputPlugin";
	public static String TCOutputConfiguration="OutputConfiguration";
	public static String TCOutputMethodElement="OutputMethodElement";
		
	private boolean trace = false;
	private boolean removeFiles = true;
	
	private File testRootSource;
	private File testRootWorking;
	private File currTestDir;
	private String outputPath;
	private Document outputDocument;
	private PrintStream testRootOut;
	private PrintStream currTestOut;
	private int executedTestCount = 0;	
		
	private static LibraryTestService service = LibraryTestService.getInstance();
	private File testFile;
	private List testCommandList = new ArrayList();
	private LibraryJunitTest junitTestCase;
	
	private MethodLibrary currentBaseLib;
	private Map allLibs = new LinkedHashMap();
	private Map allExports = new LinkedHashMap();	
	
	private static Map tcClassMap = new HashMap();
	private static Map tcTagNameMap = new HashMap();
	
	private PrintStream logPS = System.out;
	
	static {
		addToTcClassMap(TCCopyLibrary, TCCopyLibrary.class);
		addToTcClassMap(TCOpenLibrary, TCOpenLibrary.class);
		addToTcClassMap(TCExportPlugins, TCExportPlugins.class);
		addToTcClassMap(TCImportPlugins, TCImportPlugins.class);		
		addToTcClassMap(TCExportConfiguration, TCExportConfiguration.class);
		addToTcClassMap(TCImportConfiguration, TCImportConfiguration.class);
		addToTcClassMap(TCExportXml, TCExportXml.class);
		addToTcClassMap(TCImportXml, TCImportXml.class);		
		addToTcClassMap(TCCompareToLibrary, TCCompareToLibrary.class);
		addToTcClassMap(TCCompareToGoldenFile, TCCompareToGoldenFile.class);
		
		addToTcClassMap(TCNewMethodPlugin, TCNewMethodPlugin.class);
		addToTcClassMap(TCNewMethodConfiguration, TCNewMethodConfiguration.class);		
		addToTcClassMap(TCNewMethodElement, TCNewMethodElement.class);
		addToTcClassMap(TCEditMethodElement, TCEditMethodElement.class);
		
		addToTcClassMap(TCEditPlugin, TCEditMethodElement.class);
		addToTcClassMap(TCEditConfiguration, TCEditMethodElement.class);
		addToTcClassMap(TCEditContentPackage, TCEditMethodElement.class);
		
		addToTcClassMap(TCOutputLibrary, TCOutputMethodElement.class);
		addToTcClassMap(TCOutputPlugin, TCOutputMethodElement.class);
		addToTcClassMap(TCOutputConfiguration, TCOutputMethodElement.class);
		addToTcClassMap(TCOutputMethodElement, TCOutputMethodElement.class);
		addToTcClassMap(TCCircularDependencyCheck, TCCircularDependencyCheck.class);
	}
	
	private static void addToTcClassMap(String elementName, Class cls) {
		Object instance = null;
		try {
			instance = cls.newInstance();
		} catch (Exception e){
			e.printStackTrace();
		}
		if (instance instanceof TestCommand) {
			tcClassMap.put(elementName, instance);
			tcTagNameMap.put(cls, elementName);
		}		
	}

	public TestCommandMgr(boolean trace) {
		if (trace) {
			this.trace = trace;
			this.removeFiles = !trace;
		}
	}	
	
	public String getTagName(TestCommand tcInstance) {
		return (String) tcTagNameMap.get(tcInstance.getClass());
	}
	
	public void setTestRootSource(File dirFile) {
		testRootSource = dirFile;
	}
	
	public void setTestRootWorking(File dirFile) {
		boolean needToCreateOutputFile = testRootWorking == null ||
								! testRootWorking.equals(dirFile);
		
		testRootWorking = dirFile;
		
		if (trace && needToCreateOutputFile) {
			try {
				if (! testRootWorking.exists()) {					
					if (! mkDir(testRootWorking)) {
						return;
					}
				}
				if (testRootOut != null) {
					testRootOut.close();
				}
				String path = testRootWorking.getAbsolutePath() + File.separator + "traces.out";
				testRootOut = new PrintStream(new FileOutputStream(path));

			} catch (Exception e) {	
				e.printStackTrace();
			}
		}
	}
	
	private boolean mkDir(File file) {
		File pFile = file.getParentFile();		
		if (! pFile.exists()) {					
			if (! mkDir(pFile)) {
				return false;
			}
		}
		return file.mkdir();
	}
	
	public void openCurrTestDir(File dirFile) {
		currTestDir = dirFile;
		
		try {
			if (! currTestDir.exists()) {
				currTestDir.mkdir();
			}
			String path = currTestDir.getAbsolutePath() + File.separator;
			if (trace) {
				currTestOut = new PrintStream(new FileOutputStream(path + "trace.out"));
			}
			outputPath = path + "output.xml";
		} catch (Exception e) {	
			e.printStackTrace();
		}
	}
	
	public void closeCurrTestDir() {
		closeLibrary(getCurrentBaseLib());
		if (currTestOut != null) {
			currTestOut.close();
			currTestOut = null;
		}
		clearOutputDocument();
		allLibs = new LinkedHashMap();
		allExports = new LinkedHashMap();
		closeFolder(currTestDir);
	}
	
	public void doneWithAllTests(File topCleanUpFolder) {
		closeAllExportProjects();
		if (testRootOut != null) {
			testRootOut.close();
		}
		closeFolder(topCleanUpFolder);
	}
	
	public File getCurrTestDir() {
		return currTestDir;
	}
	
	public void setTest(File testFile) {
		this.testFile = testFile;		
	}	
	
	public File getTestFile() {
		return testFile;		
	}	
	
	
	public LibraryTestService getService() {
		return service;
	}
	
	public TCExeReplyList execute() {
		trace("Begin executing: " + testFile);

		TCExeReplyList result = null;
		boolean loaded = false;
		Exception loadException = null;
		try {
			loaded = loadTest();			
		} catch (Exception e) {
			loadException = e;
			e.printStackTrace();
		}
		
		if (loaded) {			
			String path = testRootWorking.getAbsolutePath() + File.separator + testFile.getName() + ".dir";
			openCurrTestDir(new File (path));
			result = executeCommands();		
			closeCurrTestDir();
		} else {
			result = new TCExeReplyListImpl();
			if (loadException != null) {
				String msg = "Test load failed: " + loadException.getMessage();
				((TCExeReplyListImpl) result).add(new TCExeReplyImpl(msg, false));
				trace(msg);
			} else {			
				trace("Test disabled");
			}
		}
		
		trace("End executing  : " + testFile);
		return result;
	}
	
	private boolean loadTest() throws Exception {
		testCommandList.clear();
		Document document = XMLUtil.loadXml(testFile);	
		Element root = document.getDocumentElement();
		if (root.getAttribute(TestCommand.AT_Disable).equals("true")) {
			return false;				
		}
		boolean verbose = root.getAttribute(TestCommand.AT_Verbose).equalsIgnoreCase("true");
		NodeList nodes = root.getChildNodes();
		int sz = nodes == null ? 0 : nodes.getLength();
		for (int i=0; i<sz; i++) {
			Node node = nodes.item(i);
			if (node instanceof Element) {
				Element element = (Element) node;
				if (verbose) {
					element.setAttribute(TestCommand.AT_Verbose, "true");
				}
				if (element.getTagName().equalsIgnoreCase(TCEnd)) {
					break;
				}
				addCommand(node);
			}
		}			
		return true;
	}
	
	public TCExeReplyList executeCommands() {
		TCExeReplyListImpl result = new TCExeReplyListImpl();
		for (int i=0; i<testCommandList.size(); i++) {
			TestCommandImpl command = (TestCommandImpl) testCommandList.get(i);
			TCExeReply reply = null;
			try {
				reply = command.execute();
			} catch (Exception e) {
				e.printStackTrace();
				reply = new TCExeReplyImpl(e.getMessage(), false);
			}
			if (reply != null) {
				result.add(reply);
				if (!reply.passing()) {
					log("\nThe following command is not passing: \n" +  command.getElement() + "\n");
				}
			}
		}			
		
		String line = testFile.getName() + " is ";
		String dirStr = "testRootSource: " + testRootSource 
						+ ", testRootWorking: " + testRootWorking;
		if (result.passing()) {
			line += "passing. " + dirStr;
		} else {
			line += "failing! " + dirStr;
		}
		result.setSummaryReply(line);
		return result;
	}
	
	public void trace(String line) {
		if (testRootOut != null) {
			testRootOut.println(line);
		}
		
		if (currTestOut != null) {
			currTestOut.println(line);
		}
	}
	
	private void addCommand(Node node) {
		if ( !(node instanceof Element)) {
			return;
		}
		Element element = (Element) node;
		if (! element.getAttribute(TestCommand.AT_Disable).equals("true")) {
			TestCommand command = newTestCommand(element.getTagName());
			if (command != null) {
				command.parse(element);
				testCommandList.add(command);
			}
		}
	}	
	
	private TestCommand newTestCommand(String comandName) {
		Object obj = tcClassMap.get(comandName);
		return obj == null ? null : newTestCommand(obj.getClass());
	}
	
	public TestCommand newTestCommand(Class cls) {
		TestCommand ret = null;
		try {
			ret = (TestCommand) cls.newInstance();
			((TestCommandImpl) ret).setOwner(this);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return ret;
	}
		
	private MethodLibrary loadBaseLib(String name, boolean ifNewThenBuildDeafaultLib) {
		if (currentBaseLib != null) {
			File file = new File(currentBaseLib.eResource().getURI().toFileString()).getParentFile();
			if (file.getName().equals(name)) {			
				return currentBaseLib;
			}
		}
		
		if (allExports.containsKey(name)) {
			throw new UnsupportedOperationException();
		}
		
		service.closeLibrary(currentBaseLib);
		
		currentBaseLib = null;
		String libPath = (String) allLibs.get(name);
		if (libPath == null) {
			libPath = currTestDir.getAbsolutePath() + File.separator + name;	
			try {
				currentBaseLib = copyFromTestLibs(libPath, name) ?  
								XMILibraryUtil.openMethodLibrary(libPath) : null;
			} catch (Exception e) {				
			}
			if (currentBaseLib == null) {
				currentBaseLib = service.createLibrary(libPath, name);
				if (ifNewThenBuildDeafaultLib) {
					buildDefaultLibrary();
				}
				trace("Base lib \"" + name + "\" is created.");
			} else {
				trace("Base lib \"" + name + "\" is copied from testLibs and laoded.");
			}
			allLibs.put(name, libPath);
		} else {
			try {
				currentBaseLib = XMILibraryUtil.openMethodLibrary(libPath);
				trace("Base lib \"" + name + "\" is loaded.");
			} catch (Exception e) {			
				trace(e.getMessage());
			}
		}
		return currentBaseLib;
	}			
	
	public MethodLibrary loadNonBaseLib(String name) {
		String libPath = (String) allLibs.get(name);
		if (libPath == null) {
			throw new UnsupportedOperationException();
		}
		return service.loadLibrary(libPath);
	}			
	
	public MethodLibrary getCurrentBaseLib() {
		return currentBaseLib;
	}
	
	public void setCurrentBaseLib(MethodLibrary lib) {
		currentBaseLib = lib;
	}
	
	public MethodLibrary loadBaseLib(String name) {
		return loadBaseLib(name, false);
	}
	
	public MethodLibrary loadBaseLibOrBuildBaseWithDefault(String name) {
		return loadBaseLib(name, true);
	}
	
	public File getImportFile(String importFolderName) {
		File file = (File) allExports.get(importFolderName);
		if (file == null) {
			file = new File(testRootSource.getAbsolutePath() + File.separator + importFolderName);
			if (file.exists() && file.isDirectory()) {
				return file;
			}
			return null;
		}
		return file;
	}
	
	public File registerExportDestination(String name, boolean isXML) {
		if (allLibs.containsKey(name)) {
			throw new UnsupportedOperationException();
		}
		File file = (File) allExports.get(name);
		if (file == null) {
			String path = currTestDir.getAbsolutePath() + File.separator + name;
			if (isXML) {
				path += File.separator + name + ".xml";
			}
			file =  new File(path);
			allExports.put(name, file);
		}
		return file;
	}	
	
	public void closeLibrary(MethodLibrary lib) {
		service.closeLibrary(lib);
		setCurrentBaseLib(null);
	}
	
	public void closeFolder(File folder) {
		if (removeFiles) {
			service.deleteFolder(folder);
		}
	}
	
	public int incExecutedTestCount() {
		return ++executedTestCount;
	}
	
	public void closeAllExportProjects() {
		for (Iterator it = allExports.values().iterator(); it.hasNext();) {
			File file = (File) it.next();
			try {
				MethodLibraryProject.closeProject(file.getAbsolutePath(), new NullProgressMonitor());
			} catch (Throwable e){					
			}
		}	
	}	
	
	public boolean compareLibs(MethodLibrary lib1,  MethodLibrary lib2) {
		return compareLibs(lib1, lib2, false);
	}
	
	public boolean compareLibs(MethodLibrary lib1,  MethodLibrary lib2, boolean useNameAs2ndId) {
		LibraryDiffAnalyzor analyzor = new LibraryDiffAnalyzor(this, lib1, lib2);
		analyzor.setGreedy(trace);
		analyzor.setUseNameAs2ndId(useNameAs2ndId);
		return analyzor.compare();
	}
	
	public boolean compareLibs(MethodLibrary lib1,  MethodLibrary lib2, int diffCount, int elemComparedCount, boolean useNameAs2ndId) {
		if (diffCount == 0 && elemComparedCount == 0) {
			return compareLibs(lib1, lib2, useNameAs2ndId);
		}
		LibraryDiffAnalyzor analyzor = new LibraryDiffAnalyzor(this, lib1, lib2);
		analyzor.setGreedy(true);
		analyzor.setUseNameAs2ndId(useNameAs2ndId);
		analyzor.compare();
		return 	analyzor.getDiffCount() == diffCount &&
			 	analyzor.getElemComparedCount() == elemComparedCount;
	}
	
	public void buildDefaultLibrary() {		
		trace("buildDefaultLibrary ->");
		
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("Plug_A");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("Config_A");
		CapabilityPattern cp1 = LibraryTestHelper.createCapabilityPattern(plugin, "CP1", config);
		CapabilityPattern cp2 = LibraryTestHelper.createCapabilityPattern(plugin, "CP2", config);		
		LibraryTestHelper.setContributes(cp2, cp1);
		
		// update the configuration
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		config.getMethodPackageSelection().add((ProcessComponent)cp1.eContainer());
		config.getMethodPackageSelection().add((ProcessComponent)cp2.eContainer());
		
		LibraryUtil.validateMethodConfiguration(config);	
		
		final String cp1_text = "cp1 main desc";
		final String cp2_text = "cp2 main desc";
			
		cp1.getPresentation().setMainDescription(cp1_text);
		cp2.getPresentation().setMainDescription(cp2_text);

		trace("buildDefaultLibrary <-");

	}
	
	public boolean copyFromTestLibs(String libPath, String relLibNamePath) {
		File source = new File(testRootSource.getAbsolutePath() + File.separator + relLibNamePath);
		String libName = source.getName();
		File libList[] = source.getParentFile().listFiles();
		for (int i=0; i<libList.length; i++) {
			File file = libList[i];
			if (libName.equals(file.getName())) {
				File target = new File(libPath);
				PluginImportingService.copyDir(source, target);
				allLibs.put(libName, libPath);
				return true;
			}
		}
		return false;
	}
	
	public Document getOutputDocument() {
		if (outputDocument == null) {			
			try {
				outputDocument = XMLUtil.createDocument();
		        Element root = outputDocument.createElement("TesterOutput");
		        outputDocument.appendChild(root);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return outputDocument;
	}
	
	private void clearOutputDocument() {
		outputDocument = null;
	}
	
	public String getOutputPath() {
		return outputPath;
	}
	
	public void log(String line) {
		logPS.println(line);
	}
	
}
