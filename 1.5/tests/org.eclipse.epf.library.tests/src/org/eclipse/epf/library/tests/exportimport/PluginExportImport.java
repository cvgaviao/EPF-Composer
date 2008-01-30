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
package org.eclipse.epf.library.tests.exportimport;

import org.eclipse.epf.library.tester.impl.ExportImportTestImpl;

/**
 * JUnit tests for plugin export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class PluginExportImport extends ExportImportTestImpl {

	public PluginExportImport(String name) {
		super(name);
	}	
		
	protected void setUp() throws Exception {
		super.setUp();		
		endSetUp();
	}
	
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
		endTearDown();
	}	
	
	public int numTestCases() {
		return 1;
	}	
	
	public void test0001_1() {
		getService().trace("Begin -> test0001_1");
		new PluginExportImportTest0001(this).execute("test0001_1");
		getService().trace("End   -> test0001_1");
	}	
	
}
