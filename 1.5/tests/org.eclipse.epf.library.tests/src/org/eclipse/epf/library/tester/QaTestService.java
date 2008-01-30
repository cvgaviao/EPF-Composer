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
import java.io.FileFilter;

import org.eclipse.epf.library.tester.iface.ITestFolderSetup;
import org.eclipse.epf.library.tester.iface.LibraryTester;
import org.eclipse.epf.library.tester.iface.TCExeReplyList;
import org.eclipse.epf.library.tester.iface.TestCommand;

/**
 * This class provides LibraryTester service to run test command files.
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class QaTestService implements ITestFolderSetup {
	
	private static String testGroupName = "TestCommandFileTests";
	private static File testGroup;		
	
	private LibraryTester tester;
	private ITestFolderSetup testFolderSetup;
	
	public static FileFilter tcFilter = new FileFilter() {
		public boolean accept(File pathname) {
			return pathname.getName().endsWith(TestCommand.TestNameExt);
		};			
	};
	
	public static FileFilter dirFilter = new FileFilter() {
		public boolean accept(File pathname) {
			return pathname.isDirectory();
		};			
	};
	
	public QaTestService(ITestFolderSetup testFolderSetup, boolean trace) {
		this.testFolderSetup = testFolderSetup;
		tester = LibraryTesterFactory.newTester(trace);
	}
	
	
	/**
	 * Return top test root source folder file
	 */
	public File getTopTestRootSource() {
		return testFolderSetup.getTopTestRootSource();
	}
	
	/**
	 * Return test home folder file
	 */
	public File getTestHome() {
		return testFolderSetup.getTestHome();
	}	
	
	public synchronized File getTestGroup() {
		if (testGroup == null) {
			String path = getTestHome().getAbsolutePath() + File.separator + testGroupName;
			testGroup = new File(path);
		}
		return testGroup;
	}
	
	/**
	 * Picks up and runs all test command files under "startingTestRootSource"
	 * folder recursively. Test working location starts from "startingTestRootWorking".
	 */
	public String runAllCases(File startingTestRootSource, File startingTestRootWorking) {	
		StringBuffer errorMsgBuf = new StringBuffer();
		traverseFolder(startingTestRootSource, startingTestRootWorking, errorMsgBuf, true);
		tester.closeCurrTestDir();
		tester.doneWithAllTests(getTestGroup());
		return errorMsgBuf.toString();
	}
	
	/**
	 * Picks up and runs all local test command files under "testRootSource"
	 * folder. Test working location is given by "testRootWorking".
	 */
	public String runLocalCases(File testRootSource, File testRootWorking) {	
		StringBuffer errorMsgBuf = new StringBuffer();
		traverseFolder(testRootSource, testRootWorking, errorMsgBuf, false);
		return errorMsgBuf.toString();
	}
	
	/**
	 * Runs the test command file "tcFile" with test root source folder
	 * given by "testRootSource" and test root working folder given by
	 * "testRootWorking".
	 */
	public String runCase(File testRootSource, File testRootWorking, File tcFile) {	
		tester.setTestRootSource(testRootSource);
		tester.setTestRootWorking(testRootWorking);
		
		StringBuffer errorMsgBuf = new StringBuffer();
		runCase(errorMsgBuf, tcFile);
		return errorMsgBuf.toString();
	}
	
	private void traverseFolder(File testRootSource, File testRootWorking, 
			StringBuffer errorMsgBuf, boolean recursuvely) {		
					
		File files[] = testRootSource.listFiles(tcFilter);
		executeTCFiles(testRootSource, testRootWorking,  files, errorMsgBuf);
		if (! recursuvely) {
			return;
		}
			
		File dirs[] = testRootSource.listFiles(dirFilter);
		int sz = dirs == null ? 0 : dirs.length;
		if (sz == 0) {
			return;
		}
		for (int i=0; i<sz; i++) {
			File dir = dirs[i];
			String path = testRootWorking.getAbsolutePath() + File.separator + dir.getName() 
								+ File.separator;
			traverseFolder(dir, new File(path), errorMsgBuf, recursuvely);
		}
	}
		
	private void executeTCFiles(File testRootSource, File testRootWorking, File tcFiles[], StringBuffer errorMsgBuf) {
		int sz = tcFiles == null ? 0 : tcFiles.length;
		if (sz == 0) {
			return;
		}
				
		tester.setTestRootSource(testRootSource);
		tester.setTestRootWorking(testRootWorking);
		
		for (int i=0; i<sz; i++) {
			File tcFile = tcFiles[i];
			runCase(errorMsgBuf, tcFile);			
		}				
	}

	private void runCase(StringBuffer errorMsgBuf, File tcFile) {	
		tester.setTest(tcFile);
		TCExeReplyList result = tester.execute();
		if (! result.passing()) {
			String msg = result.getSummaryReply();
			errorMsgBuf.append("\n" + msg);
			tester.trace(msg);
		}
	}
	

	
	
	
}
