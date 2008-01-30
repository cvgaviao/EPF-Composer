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

import org.eclipse.epf.library.edit.validation.DependencyChecker;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;

import junit.framework.TestCase;

/**
 * @author Phong Nguyen Le
 * @since  1.0
 */
public class DependencyCheckerTest extends TestCase {

	/**
	 * @param name
	 */
	public DependencyCheckerTest(String name) {
		super(name);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 * Test method for {@link org.eclipse.epf.library.edit.validation.DependencyChecker#checkCircularDependency(org.eclipse.emf.ecore.EObject, org.eclipse.emf.ecore.EStructuralFeature, java.lang.Object)}.
	 */
	public void testCheckCircularDependency() {
		/*
		plugin1 <- plugin2 <- plugin3 
					|
		            v
		           plugin4 -> plugin5		           
		*/
		MethodLibrary lib = UmaFactory.eINSTANCE.createMethodLibrary();
		
		MethodPlugin plugin1 = UmaFactory.eINSTANCE.createMethodPlugin();
		plugin1.setName("plugin1");
		lib.getMethodPlugins().add(plugin1);
		
		MethodPlugin plugin2 = UmaFactory.eINSTANCE.createMethodPlugin();
		plugin2.setName("plugin2");
		plugin2.getBases().add(plugin1);
		lib.getMethodPlugins().add(plugin2);

		MethodPlugin plugin3 = UmaFactory.eINSTANCE.createMethodPlugin();
		plugin3.setName("plugin3");
		plugin3.getBases().add(plugin2);
		lib.getMethodPlugins().add(plugin3);
		
		MethodPlugin plugin4 = UmaFactory.eINSTANCE.createMethodPlugin();
		plugin4.setName("plugin4");
		lib.getMethodPlugins().add(plugin4);
		
		MethodPlugin plugin5 = UmaFactory.eINSTANCE.createMethodPlugin();
		plugin5.setName("plugin5");
		lib.getMethodPlugins().add(plugin5);

		plugin2.getBases().add(plugin4);
		plugin4.getBases().add(plugin5);


		assertFalse(DependencyChecker.checkCircularDependency(plugin1, UmaPackage.Literals.METHOD_PLUGIN__BASES, plugin3).isOK());
		assertFalse(DependencyChecker.checkCircularDependency(plugin5, UmaPackage.Literals.METHOD_PLUGIN__BASES, plugin3).isOK());
		assertTrue(DependencyChecker.checkCircularDependency(plugin5, UmaPackage.Literals.METHOD_PLUGIN__BASES, plugin1).isOK());
			
	}

}
