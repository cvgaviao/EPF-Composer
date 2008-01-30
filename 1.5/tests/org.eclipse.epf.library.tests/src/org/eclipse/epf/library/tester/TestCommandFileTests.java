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
import java.util.Date;

import junit.framework.TestCase;

import com.ibm.icu.text.SimpleDateFormat;
import com.ibm.icu.util.Calendar;

/**
 * JUNIT test on test command files
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TestCommandFileTests extends TestCase {
	
	private QaTestService service = new	QaTestService(new TestFolderSetup(), false);
	
	public TestCommandFileTests(String name) {
		super(name);
	}						
			
	/**
	 * This test picks up and runs all test command files under the
	 * folder service.getTopTestRootSource() recursively. 
	 */ 
	public void testAllCases() {
		runTestAllCases(service);
	}
	
	public static void runTestAllCases(QaTestService service) {
		String path = service.getTestGroup().getAbsolutePath();
		
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss:SSS"); //$NON-NLS-1$
		String dtStr = sdf.format(new Date());
		dtStr = dtStr.replace(' ', '_');
		dtStr = dtStr.replace(':', '-');
		
		path += File.separator + dtStr + File.separator;
		String errorMsg = service.runAllCases(service.getTopTestRootSource(), new File(path));
		assertTrue(errorMsg, errorMsg.length() == 0);
	}
		
}
