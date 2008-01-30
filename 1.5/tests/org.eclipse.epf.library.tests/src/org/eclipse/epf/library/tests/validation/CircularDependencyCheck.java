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
package org.eclipse.epf.library.tests.validation;

import org.eclipse.epf.library.tester.LibraryJunitTestService;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.iface.TestCommand;
import org.eclipse.epf.library.tester.impl.ExportImportTestImpl;
import org.eclipse.epf.library.tester.impl.testcommands.TCNewMethodElement;
import org.eclipse.epf.library.tester.impl.testcommands.TCOpenLibrary;

/**
 * JUnit tests for plugin export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class CircularDependencyCheck extends ExportImportTestImpl {

	public CircularDependencyCheck(String name) {
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
	
	public void test0001_169501() {
		LibraryJunitTestService service = getService();
		
		TestCommand tcs[] = new TestCommand[8];
		
		tcs[0] = service.newTestCommand(TCOpenLibrary.class);
		tcs[0].setAttribute(TestCommand.AT_LibFolderName, "169501");
		
/*		tcs[1] = service.newTestCommand(TCNewMethodElement.class);
		tcs[1].setAttribute(TestCommand.AT_Type, "MethodPlugin");
		tcs[1].setAttribute(TestCommand.AT_Name, "Plug A");	*/											
		
		boolean passing = true;
		for (int i=0; i<tcs.length; i++) {
			if (tcs[i] != null) {
				TCExeReply reply = tcs[i].execute();
				if (reply != null && ! reply.passing()) {
					passing = false;
				}
			}
		}
		assertTrue(passing);
	}	
	
}
