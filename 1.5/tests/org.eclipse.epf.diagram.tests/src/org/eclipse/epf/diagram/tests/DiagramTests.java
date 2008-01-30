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
package org.eclipse.epf.diagram.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * JUnit test suite for org.eclipse.epf.diagram.
 * 
 * @author Shilpa Toraskar
 * @since 1.2
 */
public class DiagramTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(DiagramTests.class.getPackage().getName());
		//$JUnit-BEGIN$
		suite.addTestSuite(DiagramUIServiceTest.class);
		
		//$JUnit-END$
		return suite;
	}

}
