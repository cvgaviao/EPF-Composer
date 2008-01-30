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


/**
 * This interface should be implemented by all TestCase
 * classes that want to use LibraryJunitTestService. 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public interface LibraryJunitTest {
	
	/*
	 * Number of test cases in the implementing
	 * TestCase class. It is used by LibraryJunitTestService
	 * to determine when to clear up some tester resources.
	 */
	int numTestCases();
	
	/*
	 * Name of the test class instance
	 */
	String getName();
	
}
