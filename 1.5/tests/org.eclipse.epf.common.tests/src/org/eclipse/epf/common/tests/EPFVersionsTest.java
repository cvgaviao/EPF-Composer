package org.eclipse.epf.common.tests;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.epf.common.serviceability.EPFVersion;
import org.eclipse.epf.common.serviceability.EPFVersions;
import org.eclipse.epf.common.serviceability.VersionUtil;

public class EPFVersionsTest extends TestCase {

	public EPFVersionsTest(String name) {
		super(name);
	}

	public void testGetCurrentVersion() {
		EPFVersions versions = (EPFVersions)VersionUtil.getVersions(EPFVersions.TOOL_ID);
		assertEquals(new EPFVersion("1.2","1.0.4.1","1.0.1"), versions.getCurrentVersion());
	}

	public void testGetVersion() {
		EPFVersions versions = (EPFVersions)VersionUtil.getVersions(EPFVersions.TOOL_ID);
		assertEquals(new EPFVersion("1.0","1.0.3.0","1.0.0"), versions.getVersion("1.0"));
	}

	public void testGetAllVersions() {
		EPFVersions versions = (EPFVersions)VersionUtil.getVersions(EPFVersions.TOOL_ID);
		List expectedList = new ArrayList();
		expectedList.add(new EPFVersion("1.0","1.0.3.0","1.0.0"));
		expectedList.add(new EPFVersion("1.1","1.0.4.0","1.0.0"));
		expectedList.add(new EPFVersion("1.2","1.0.4.1","1.0.1"));
		assertEquals(expectedList, versions.getAllVersions());
	}

}
