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
package org.eclipse.epf.library.tests;

import java.io.File;

import junit.framework.TestCase;

import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.util.ModelStorage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.UmaFactory;

/**
 * JUnit tests for the org.eclipse.epf.library plug-in.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class LibraryServiceTest extends TestCase {

	private static final String LIBRARY_XMI = "library.xmi"; //$NON-NLS-1$	

	private static final String PLUGIN_XMI = "plugin.xmi"; //$NON-NLS-1$	

	private static final String TEST_LIBRARY_1 = "library_1"; //$NON-NLS-1$

	private static final String TEST_PLUGIN_1 = "plugin_1"; //$NON-NLS-1$	

	/**
	 * Creates a new instance.
	 */
	public LibraryServiceTest(String name) {
		super(name);
	}

	/**
	 * Tests the creation of a method library.
	 */
	public static void testCreateMethodLibrary() {
		try {
			LibraryTestHelper
					.createTestMethodLibrary(LibraryTestHelper.TEMP_TEST_DIR
							+ File.separator + TEST_LIBRARY_1);
			File libFile = new File(LibraryTestHelper.TEMP_TEST_DIR
					+ File.separator + TEST_LIBRARY_1 + File.separator
					+ LIBRARY_XMI);
			assertTrue(libFile.exists());
		} catch (Exception e) {
			fail();
		}
	}

	/**
	 * Tests the creation of a method plug-in.
	 */
	public static void testCreateMethodPlugin() {
		try {
			MethodPlugin plugin = UmaFactory.eINSTANCE.createMethodPlugin();
			plugin.setName(TEST_PLUGIN_1);

			ILibraryManager manager = (ILibraryManager) LibraryService
					.getInstance().getCurrentLibraryManager();
			if (manager != null) {
				manager.addMethodPlugin(ModelStorage.initialize(plugin));
			}

			File pluginFile = new File(LibraryTestHelper.TEMP_TEST_DIR
					+ File.separator + TEST_LIBRARY_1 + File.separator
					+ TEST_PLUGIN_1 + File.separator + PLUGIN_XMI);
			assertTrue(pluginFile.exists());

			LibraryTestHelper.closeLibrary();
		} catch (Exception e) {
			fail();
		}
	}
}
