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
package org.eclipse.epf.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.eclipse.epf.common.tests.CommonTests;
import org.eclipse.epf.diagram.tests.DiagramTests;
import org.eclipse.epf.library.tests.LibraryTests;

/**
 * JUnit Tests for EPF Composer.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(AllTests.class.getPackage().getName());
		//$JUnit-BEGIN$		
		suite.addTest(CommonTests.suite());
		suite.addTest(DiagramTests.suite());
		suite.addTest(LibraryTests.suite());		
		//$JUnit-END$		
		return suite;
	}

}
