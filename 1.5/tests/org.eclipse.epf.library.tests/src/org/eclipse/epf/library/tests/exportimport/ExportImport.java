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

import java.io.File;
import java.io.FileFilter;

import junit.framework.TestCase;

import org.eclipse.epf.library.tester.QaTestService;
import org.eclipse.epf.library.tester.TestFolderSetup;
import org.eclipse.epf.library.tests.TestsPlugin;

/**
 * JUnit tests for export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class ExportImport extends TestCase {

	
	private static final File testRootSource = new File (TestsPlugin.getDefault().getInstallPath() +  
		"Libraries" + File.separator + "1.0.0" + File.separator + "ExportImport");
	
	private String testRootParent =  System.getProperty("user.home") + File.separator 
									+ "EPF" + File.separator + "test" + File.separator;

	
	
	private QaTestService service = new	QaTestService(new TestFolderSetup(), true);
	
	public ExportImport(String name) {
		super(name);
	}
	
	public void testCase() {		
		final String fileName = "1stTest.tc";
		FileFilter filter = new FileFilter() {
			public boolean accept(File pathname) {
				return pathname.getName().equalsIgnoreCase(fileName);
			};			
		};
		
		File files[] = testRootSource.listFiles(filter);
		if (files == null || files.length == 0) {
			assertTrue(fileName + " cannot be found!", false);
			return;
		}

		File testRootWorking = new File(testRootParent + "ExportImportOneCase");
		String errorMsg = service.runCase(testRootSource, testRootWorking, files[0]);
		assertTrue(errorMsg, errorMsg.length() == 0);
	}	
			
	public void testLocalCases() {
		File testRootWorking = new File(testRootParent + "ExportImport");		
		String errorMsg = service.runLocalCases(testRootSource, testRootWorking);
		assertTrue(errorMsg, errorMsg.length() == 0);
	}	
				
				
}
