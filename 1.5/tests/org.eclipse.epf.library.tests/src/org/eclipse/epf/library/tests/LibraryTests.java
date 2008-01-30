//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//---------------------------------------------------------------------
package org.eclipse.epf.library.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.eclipse.epf.library.tester.TestCommandFileTests;
import org.eclipse.epf.library.tests.exportimport.ConfigExportImport;
import org.eclipse.epf.library.tests.exportimport.PluginExportImport;
import org.eclipse.epf.library.tests.exportimport.XmlExportImport;
import org.eclipse.epf.library.tests.variability.ActivityVariabilityTest;
import org.eclipse.epf.library.tests.variability.AttributeFeatureTest;
import org.eclipse.epf.library.tests.variability.CopyrightTest;
import org.eclipse.epf.library.tests.variability.Incoming01FeatureTest;
import org.eclipse.epf.library.tests.variability.Incoming0nFeatureTest;
import org.eclipse.epf.library.tests.variability.Outgoing01FeatureTest;
import org.eclipse.epf.library.tests.variability.Outgoing0nFeatureTest;
import org.eclipse.epf.library.tests.variability.RegressionTest;

/**
 * JUnit tests for the org.eclipse.epf.library class.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class LibraryTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(LibraryTests.class.getPackage().getName());
		//$JUnit-BEGIN$
		suite.addTestSuite(LibraryServiceTest.class);
		suite.addTestSuite(Outgoing01FeatureTest.class);
		suite.addTestSuite(Outgoing0nFeatureTest.class);
		suite.addTestSuite(Incoming01FeatureTest.class);
		suite.addTestSuite(Incoming0nFeatureTest.class);
		suite.addTestSuite(AttributeFeatureTest.class);
		suite.addTestSuite(CopyrightTest.class);
		suite.addTestSuite(RegressionTest.class);	
		suite.addTestSuite(ActivityVariabilityTest.class);	
		
		suite.addTestSuite(TestCommandFileTests.class);
		suite.addTestSuite(PluginExportImport.class);
		suite.addTestSuite(ConfigExportImport.class);	
		suite.addTestSuite(XmlExportImport.class);
		
		//$JUnit-END$
		return suite;
	}

}
