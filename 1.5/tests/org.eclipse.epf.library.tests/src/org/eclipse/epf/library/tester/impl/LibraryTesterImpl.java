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

import org.eclipse.epf.library.tester.iface.LibraryTester;
import org.eclipse.epf.library.tester.iface.TCExeReplyList;
import org.eclipse.epf.library.tester.iface.TestCommand;

/**
 * Used in JUnit tests 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class LibraryTesterImpl implements LibraryTester {
	
	private TestCommandMgr testCommandMgr;
	
	public LibraryTesterImpl(boolean trace) {
		testCommandMgr = new TestCommandMgr(trace);
	}			
	
	public void setTestRootSource(File dirFile) {
		testCommandMgr.setTestRootSource(dirFile);
	}
	
	public void setTestRootWorking(File dirFile) {
		testCommandMgr.setTestRootWorking(dirFile);
	}
	
	public void openCurrTestDir(File dirFile) {
		testCommandMgr.openCurrTestDir(dirFile);
	}
	
	public void closeCurrTestDir() {
		testCommandMgr.closeCurrTestDir();
	}
	
	public void doneWithAllTests(File topCleanUpFolder) {
		testCommandMgr.doneWithAllTests(topCleanUpFolder);
	}
	
	public File getCurrTestDir() {
		return testCommandMgr.getCurrTestDir();
	}
	
	public void setTest(File testFile) {
		testCommandMgr.setTest(testFile);
	}		
	
	public TCExeReplyList execute() {
		return testCommandMgr.execute();
	}	
	
	public void trace(String line) {
		testCommandMgr.trace(line);
	}	
	
	public TestCommand newTestCommand(Class cls) {
		return testCommandMgr.newTestCommand(cls);
	}		
	
	public int incExecutedTestCount() {
		return testCommandMgr.incExecutedTestCount();
	}
	
}
