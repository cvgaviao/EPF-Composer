package org.eclipse.epf.common.tests;

import junit.framework.TestCase;

import org.eclipse.epf.common.service.versioning.EPFVersion;
import org.eclipse.epf.common.service.versioning.EPFVersions;
import org.eclipse.epf.common.service.versioning.VersionUtil;
import org.osgi.framework.Version;

public class EPFVersionTest extends TestCase {

	public EPFVersionTest(String name) {
		super(name);
	}

	public void testGetToolVersion() {
		EPFVersion version = VersionUtil.getVersions(EPFVersions.TOOL_ID).getCurrentVersion();
		assertEquals(new Version("1.2"),version.getToolVersion());
	}

	public void testGetLibraryVersion() {
		EPFVersion version = VersionUtil.getVersions(EPFVersions.TOOL_ID).getCurrentVersion();
		assertEquals(new Version("1.0.4.1"),version.getLibraryVersion());
	}

	public void testGetXMLSchemaVersion() {
		EPFVersion version = VersionUtil.getVersions(EPFVersions.TOOL_ID).getCurrentVersion();
		assertEquals(new Version("1.0.1"),version.getXMLSchemaVersion());
	}

	public void testCompareToolVersionTo() {
		EPFVersion version = VersionUtil.getVersions(EPFVersions.TOOL_ID).getCurrentVersion();
		assertTrue(version.compareToolVersionTo(new Version("1.2")) == 0);
		assertTrue(version.compareToolVersionTo(new Version("1.3")) < 0);
		assertTrue(version.compareToolVersionTo(new Version("0.9")) > 0);
	}

	public void testCompareLibraryVersionTo() {
		EPFVersion version = VersionUtil.getVersions(EPFVersions.TOOL_ID).getCurrentVersion();
		assertTrue(version.compareLibraryVersionTo(new Version("1.0.4.1")) == 0);
		assertTrue(version.compareLibraryVersionTo(new Version("1.0.4.2")) < 0);
		assertTrue(version.compareLibraryVersionTo(new Version("1.0.2.0")) > 0);
	}

	public void testCompareXMLSchemaVersionTo() {
		EPFVersion version = VersionUtil.getVersions(EPFVersions.TOOL_ID).getCurrentVersion();
		assertTrue(version.compareXMLSchemaVersionTo(new Version("1.0.1")) == 0);
		assertTrue(version.compareXMLSchemaVersionTo(new Version("1.0.2")) < 0);
		assertTrue(version.compareXMLSchemaVersionTo(new Version("0.9.9")) > 0);
	}

}
