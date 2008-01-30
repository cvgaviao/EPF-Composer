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
import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.tester.iface.LibraryJunitTest;
import org.eclipse.epf.library.tester.iface.LibraryTester;
import org.eclipse.epf.library.tester.iface.TestCommand;
import org.eclipse.epf.library.tester.iface.TestTracer;
import org.eclipse.epf.library.tests.TestsPlugin;

/**
 * This class provides LibraryTester service and is used in LibraryJunitTest.  
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class LibraryJunitTestService extends TestFolderSetup implements TestTracer {
	
	private static String testGroupName = "JunitTests";
	private static File testGroup;
		
	private LibraryTester tester;
	
	private static Map instances = new HashMap();
	private File topCleanUpFolder;
	
	private LibraryJunitTestService () {
		tester = LibraryTesterFactory.newTester(false);
	}
	
	public static LibraryJunitTestService getInstance(LibraryJunitTest testCase) {
		Class testClass = testCase.getClass(); 
		LibraryJunitTestService instance = (LibraryJunitTestService) instances.get(testClass);
		if (instance == null) {
			instance = new LibraryJunitTestService();
			instances.put(testClass, instance);
		}
		return instance;
	}
	
	public synchronized File getTestGroup() {
		if (testGroup == null) {
			String path = getTestHome().getAbsolutePath() + File.separator + testGroupName;
			testGroup = new File(path);
		}
		return testGroup;
	}
	
	/**
	 * Called from LibraryJunitTest's setUp method to update tester's
	 * folder setup.
	 */
	public void setUp(File testRootSource, File testRootWorking, File testDir, LibraryJunitTest testCase) {
		tester.setTestRootSource(testRootSource);
		tester.setTestRootWorking(testRootWorking);
		File parent = testRootWorking.getParentFile();
		if (topCleanUpFolder == null) {
			topCleanUpFolder = parent;
		} else if (! topCleanUpFolder.equals(parent)){
			//Won't happen for now
		}
		tester.openCurrTestDir(testDir);
	}	
	
	/**
	 * Called from LibraryJunitTest's tearDown method to clear some
	 * resources in tester.
	 */
	public void tearDown(LibraryJunitTest testCase) {
		tester.closeCurrTestDir();
		if (tester.incExecutedTestCount() == testCase.numTestCases()) {
			tester.doneWithAllTests(getTestGroup());
		}
	}
	
	/**
	 * Create a test command instance of the tpye "cls".
	 */
	public TestCommand newTestCommand(Class cls) {
		return tester == null ? null : tester.newTestCommand(cls);
	}
	
	/**
	 * Tracing method that can be called by in LibraryJunitTest's code.
	 */
	public void trace(String line) {
		if (tester != null) {
			tester.trace(line);
		}
	}
	
}
