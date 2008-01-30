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
package org.eclipse.epf.library.tester.iface;

import java.io.File;

import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.uma.MethodLibrary;

/**
 * Used in JUnit tests 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public interface LibraryTester extends TestTracer {	
	
	TestCommand newTestCommand(Class cls);	
	
	void setTestRootSource(File dirFile);
	
	void setTestRootWorking(File dirFile);
	
	void openCurrTestDir(File dirFile);
	
	void closeCurrTestDir();
	
	void doneWithAllTests(File topCleanUpFolder);	
		
	void setTest(File testFile);			
	
	int incExecutedTestCount();
	
	void trace(String line);
	
	TCExeReplyList execute();
	
}
