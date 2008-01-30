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
package org.eclipse.epf.library.tests.variability;

import junit.framework.TestCase;

import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.library.tests.LibraryTestHelper;

/**
 * 
 * @author Jinhua Xi
 * @since 1.0
 *
 */
public abstract class VariablityBaseTestCase extends TestCase {

	public VariablityBaseTestCase(String name) {
		super(name);
	}
	
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
		LibraryTestHelper.createTestMethodLibrary();
		
		// activate the Authoring plugins to initialize something
		// the reason to put it after the library is created is to avoid the Open Library dialog
		AuthoringUIPlugin.getDefault();

	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
		
		LibraryTestHelper.closeLibrary();
	}
	
}
