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
package org.eclipse.epf.library.tester;

import java.io.File;

import junit.framework.TestCase;

/**
 * JUNIT test on runing a test command file in trace mode
 * for debug purpose.
 * The location of test file is hard coded for now.
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TestCommandFileTest extends TestCase {
	
	/**
	 * Test file path relative to topTestRootSource
	 */
	private static final String tcFilePathFromTopRootSource[] = {
/*		"TestCommands",
		"newMethodPlugin",
		"newMethodPlugin.tc"*/

		"ExportImport",
		"Bugs_1",
		"143867",
		"143867.tc"		
		
//		"TestCommands",
//		"newMethodElements",
//		"newMethodElements.tc"
		
/*		"TestCommands",
		"kelvinCase.tc"*/
		
/*		"Validation",
		"Bugs",
		"171028.tc"*/
	};
		
	private QaTestService service = new	QaTestService(new TestFolderSetup(), true);
	
	public TestCommandFileTest(String name) {
		super(name);
	}
	
	/**
	 * This test runs the given test command file.
	 */ 
	public void testOneCase() {
		runTestOneCase(service, tcFilePathFromTopRootSource);
	}
	
	public static void runTestOneCase(QaTestService service, String tcFilePathFromTopRootSource[]) {
		String path = service.getTestGroup().getAbsolutePath();
		//path += File.separator + Long.toHexString(Calendar.getInstance().getTimeInMillis()) + File.separator;		
		File testRootWorking = new File(path);
		LibraryTestService.getInstance().deleteFolder(testRootWorking);
		
		File topTestRootSource = service.getTopTestRootSource();
		
		path = topTestRootSource.getAbsolutePath();
		for (int i=0; i<tcFilePathFromTopRootSource.length; i++) {
			path += File.separator + tcFilePathFromTopRootSource[i];
		}
		File tcFile = new File(path);		
		
		String errorMsg = service.runCase(tcFile.getParentFile(), testRootWorking, tcFile);
		assertTrue(errorMsg, errorMsg.length() == 0);
	}
		
}
