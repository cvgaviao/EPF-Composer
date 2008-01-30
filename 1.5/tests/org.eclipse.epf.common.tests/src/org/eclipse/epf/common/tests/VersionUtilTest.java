package org.eclipse.epf.common.tests;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.eclipse.epf.common.serviceability.EPFVersions;
import org.eclipse.epf.common.serviceability.VersionUtil;
import org.eclipse.epf.common.utils.FileUtil;

public class VersionUtilTest extends TestCase {

	public VersionUtilTest(String name) {
		super(name);
	}

	public void testGetAllToolIDs() {
		Set toolIDs = VersionUtil.getAllToolIDs();
		for (Iterator iter = toolIDs.iterator();iter.hasNext();) {
			String toolID = (String)iter.next();
			assertTrue(toolID.equals("epf") || toolID.equals("rmc"));
		}
	}

	public void testGetVersions() {
		EPFVersions versions = VersionUtil.getVersions(EPFVersions.TOOL_ID);
		assertNotNull(versions);
	}

	public void testIsVersionCheckingDisabled() {
		assertFalse(VersionUtil.isVersionCheckingDisabled());
	}
	
	public void testReadVersionsFromFile() {
		// test no-versions-in-file case
		File xmiFile = createTestXMIFile("");
		if (xmiFile == null) {
			fail();
		}
		Map versions = VersionUtil.readVersionsFromFile(xmiFile);
		assertNull(versions);
		xmiFile.delete();
	
		// test epf-only case
		xmiFile = createTestXMIFile("epf:version=\"1.0.0\"");
		if (xmiFile == null) {
			fail();
		}
		versions = VersionUtil.readVersionsFromFile(xmiFile);
		String epfVer = (String)versions.get("epf");
		assertTrue(epfVer.equals("1.0.0"));
		xmiFile.delete();
	
		// test multiple versions case
		xmiFile = createTestXMIFile("epf:version=\"1.0.0\" rmc:version=\"7.1.0\"");
		if (xmiFile == null) {
			fail();
		}
		versions = VersionUtil.readVersionsFromFile(xmiFile);
		epfVer = (String)versions.get("epf");
		String rmcVer = (String)versions.get("rmc");
		assertTrue(epfVer.equals("1.0.0"));
		assertTrue(rmcVer.equals("7.1.0"));
		xmiFile.delete();
		
		//==========================================
		// test XML file
		//==========================================
		// test no-versions-in-file case
		File xmlFile = createTestXMLFile("");
		if (xmlFile == null) {
			fail();
		}
		versions = VersionUtil.readVersionsFromFile(xmlFile);
		assertNull(versions);
		xmlFile.delete();
	
		// test epf-only case
		xmlFile = createTestXMLFile("tool=\"epf=1.0.0\"");
		if (xmlFile == null) {
			fail();
		}
		versions = VersionUtil.readVersionsFromFile(xmlFile);
		epfVer = (String)versions.get("epf");
		assertTrue(epfVer.equals("1.0.0"));
		xmlFile.delete();
	
		// test multiple versions case
		xmlFile = createTestXMLFile("tool=\"epf=1.0.0;rmc=7.1.0\"");
		if (xmlFile == null) {
			fail();
		}
		versions = VersionUtil.readVersionsFromFile(xmlFile);
		epfVer = (String)versions.get("epf");
		rmcVer = (String)versions.get("rmc");
		assertTrue(epfVer.equals("1.0.0"));
		assertTrue(rmcVer.equals("7.1.0"));
		xmlFile.delete();
	}
	
	private File createTestXMIFile(String versionAttributes) {
		try {
			File xmiFile = File.createTempFile("EPF_JUNIT_", ".xmi");
			String xmiText = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
			    "\n<xmi:XMI xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\"" +
			    " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
			    " xmlns:org.eclipse.epf.uma=\"http://www.eclipse.org/epf/uma/1.0.3/uma.ecore\"" + 
			    " xmlns:org.eclipse.epf.uma.resourcemanager=\"http:///org/eclipse/epf/uma/resourcemanager.ecore\" " + 
			    versionAttributes + ">\n...\n</xmi:XMI>\n";
			FileUtil.writeUTF8File(xmiFile.getAbsolutePath(), xmiText);
			return xmiFile;
		} catch (IOException ex) {
			return null;
		}
	}

	private File createTestXMLFile(String versionAttributes) {
		try {
			File xmlFile = File.createTempFile("EPF_JUNIT_", ".xml");
			String xmlText = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
				"\n<uma:MethodLibrary xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
				"xmlns:uma=\"http://www.eclipse.org/epf/uma/1.0.3\" " +
				versionAttributes +
				" name=\"a\" briefDescription=\"\" id=\"_HVnG8DL5Edu42N0XS7onUg\"" +
				"orderingGuide=\"\" suppressed=\"false\" authors=\"\" changeDescription=\"\" version=\"\">" +
				"\n...\n</uma:MethodLibrary>";
			FileUtil.writeUTF8File(xmlFile.getAbsolutePath(), xmlText);
			return xmlFile;
		} catch (IOException ex) {
			return null;
		}
	}

	
}
