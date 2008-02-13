//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.tests;

import java.io.File;

import junit.framework.TestCase;

import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.LibraryServiceException;
import org.eclipse.epf.library.xmi.XMILibraryUtil;
import org.eclipse.epf.persistence.MultiFileSaveUtil;
import org.eclipse.epf.uma.MethodLibrary;

/**
 * The abstract base class for all JUnit test cases that needs to open a method library
 * 
 * @author Phong Nguyen Le
 * @since 1.0
 */
public abstract class AbstractLibraryTestCase extends TestCase {

	protected MethodLibrary library;

	protected boolean deleteLibraryOnExit = false;

	public AbstractLibraryTestCase(String name) {
		super(name);
	}

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
		openLibrary();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		if (deleteLibraryOnExit) {
			LibraryTestHelper.closeLibrary();
		} else {
			LibraryService.getInstance().closeCurrentMethodLibrary();
		}
		super.tearDown();
	}

	protected MethodLibrary openLibrary() throws LibraryServiceException {
		String libPath = System.getProperty("epf.library");
		if(libPath == null) {
			String path = TestsPlugin.getDefault().getInstallPath() + File.separator + "Libraries" + File.separator + "OpenUP";
			if(new File(path, MultiFileSaveUtil.DEFAULT_LIBRARY_MODEL_FILENAME).exists()) {
				libPath = path;
			}
		}
		if (libPath != null) {
			library = XMILibraryUtil.openMethodLibrary(libPath);
			LibraryService.getInstance().setCurrentMethodLibrary(library);
		} else {
			library = LibraryTestHelper.createTestMethodLibrary();
			deleteLibraryOnExit = true;
		}
		return library;
	}

	protected MethodLibrary reopenLibrary() throws LibraryServiceException {
		return LibraryService.getInstance().reopenCurrentMethodLibrary();
	}

}
