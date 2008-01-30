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

import junit.framework.TestCase;

import org.eclipse.epf.library.tester.LibraryJunitTestService;
import org.eclipse.epf.library.tester.LibraryTesterFactory;
import org.eclipse.epf.library.tester.iface.LibraryJunitTest;

/**
 * Library junit test base class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public abstract class LibraryJunitTestImpl extends TestCase implements LibraryJunitTest {

	private LibraryJunitTestService service;	
	
	public LibraryJunitTestImpl(String name) {
		super(name);
		service = LibraryJunitTestService.getInstance(this);
	}
	
	protected abstract File getTestRootSource();
	
	protected abstract File getTestRootWorking();
	
	protected abstract File getDefaultTestDir();
	
	
	public LibraryJunitTestService getService() {
		return service;
	}	
	
	protected void setUp() throws Exception {
		super.setUp();
		service.setUp(getTestRootSource(), getTestRootWorking(), getDefaultTestDir(), this);
	}	
	
	protected void tearDown() throws Exception {
		super.tearDown();
		service.tearDown(this);
	}		
	
	protected final void endSetUp() {
		service.trace("endSetUp: " + getName());
	}
	
	protected final void endTearDown() {		
		service.trace("endTearDown: " + getName());
		service.trace("");	
	}		
	
	public int numTestCases() {
		return 0;
	}
	
}
