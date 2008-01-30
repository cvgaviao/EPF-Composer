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

import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.Task;

/**
 * test case for copyright text realization
 * 
 * When a contributing element has a copyright, 
 * it should be appended to the copyright of the parent element on the final published page.

	Also, when an extending element has a copyright, 
	the published page for the extending element should 
	have the parent element's copyright followed by the extending elements copyright.

	For replaces, the replacing copyright should be present.
	
 * @author Jinhua Xi
 * @since 1.0
 *
 */
public class CopyrightTest extends VariablityBaseTestCase {

	private static final String copyright_text_1 = "================ This is copyright text 1 ===================";
	private static final String copyright_text_2 = "================ This is copyright text 2 ===================";
	
	public CopyrightTest(String name) {
		super(name);
	}
	
	public void testCopyright() {
				
		MethodPlugin plugin1 = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg1 = LibraryTestHelper.createContentPackage(plugin1, "p1");
		SupportingMaterial copyright_1 = LibraryTestHelper.createSupportingMaterial(pkg1, "copyright 1");
		copyright_1.getPresentation().setMainDescription(copyright_text_1);
		plugin1.setCopyrightStatement(copyright_1);
		
		MethodPlugin plugin2 = LibraryTestHelper.createMethodPlugin("plugin 2");
		ContentPackage pkg2 = LibraryTestHelper.createContentPackage(plugin2, "p2");
		SupportingMaterial copyright_2 = LibraryTestHelper.createSupportingMaterial(pkg2, "copyright 2");
		copyright_2.getPresentation().setMainDescription(copyright_text_2);
		plugin2.setCopyrightStatement(copyright_2);
		plugin2.getBases().add(plugin1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin1);
		config.getMethodPackageSelection().add(pkg1);
		config.getMethodPluginSelection().add(plugin2);
		config.getMethodPackageSelection().add(pkg2);
		LibraryUtil.validateMethodConfiguration(config);	
		
		Task t1 = LibraryTestHelper.createTask(pkg1, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg2, "t2");

		String text = ConfigurationHelper.getCopyrightText(t1, config);
		int index1 = text.indexOf(copyright_text_1);
		assertTrue(index1>=0);
		
		text = ConfigurationHelper.getCopyrightText(t2, config);
		int index2 = text.indexOf(copyright_text_2);
		assertTrue(index2>=0);
	
	}
	
	public void testCopyright_with_contributors() {
		MethodPlugin plugin1 = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg1 = LibraryTestHelper.createContentPackage(plugin1, "p1");
		SupportingMaterial copyright_1 = LibraryTestHelper.createSupportingMaterial(pkg1, "copyright 1");
		copyright_1.getPresentation().setMainDescription(copyright_text_1);
		plugin1.setCopyrightStatement(copyright_1);
		
		MethodPlugin plugin2 = LibraryTestHelper.createMethodPlugin("plugin 2");
		ContentPackage pkg2 = LibraryTestHelper.createContentPackage(plugin2, "p2");
		SupportingMaterial copyright_2 = LibraryTestHelper.createSupportingMaterial(pkg2, "copyright 2");
		copyright_2.getPresentation().setMainDescription(copyright_text_2);
		plugin2.setCopyrightStatement(copyright_2);
		plugin2.getBases().add(plugin1);

		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin1);
		config.getMethodPackageSelection().add(pkg1);
		config.getMethodPluginSelection().add(plugin2);
		config.getMethodPackageSelection().add(pkg2);
		LibraryUtil.validateMethodConfiguration(config);	
		
		Task t1 = LibraryTestHelper.createTask(pkg1, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg2, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg2, "t3");

		LibraryTestHelper.setContributes(t2, t1);
		LibraryTestHelper.setContributes(t3, t1);
		
		String text = ConfigurationHelper.getCopyrightText(t1, config);
		int index1 = text.indexOf(copyright_text_1);		
		int index2 = text.indexOf(copyright_text_2);
		int index3 = text.indexOf(copyright_text_2, index2 + copyright_text_2.length());
		assertTrue(index1>=0);
		assertTrue(index2>=0);
		assertTrue(index3<0); // t2 and t3 has the same copyright, should only show once
		
	}
	
	public void testCopyright_with_replacer() {
		MethodPlugin plugin1 = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg1 = LibraryTestHelper.createContentPackage(plugin1, "p1");
		SupportingMaterial copyright_1 = LibraryTestHelper.createSupportingMaterial(pkg1, "copyright 1");
		copyright_1.getPresentation().setMainDescription(copyright_text_1);
		plugin1.setCopyrightStatement(copyright_1);
		
		MethodPlugin plugin2 = LibraryTestHelper.createMethodPlugin("plugin 2");
		ContentPackage pkg2 = LibraryTestHelper.createContentPackage(plugin2, "p2");
		SupportingMaterial copyright_2 = LibraryTestHelper.createSupportingMaterial(pkg2, "copyright 2");
		copyright_2.getPresentation().setMainDescription(copyright_text_2);
		plugin2.setCopyrightStatement(copyright_2);
		plugin2.getBases().add(plugin1);

		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin1);
		config.getMethodPackageSelection().add(pkg1);
		config.getMethodPluginSelection().add(plugin2);
		config.getMethodPackageSelection().add(pkg2);
		LibraryUtil.validateMethodConfiguration(config);	
		
		Task t1 = LibraryTestHelper.createTask(pkg1, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg2, "t2");

		LibraryTestHelper.setReplaces(t2, t1);
		
		String text = ConfigurationHelper.getCopyrightText(t2, config);
		int index1 = text.indexOf(copyright_text_1);		
		int index2 = text.indexOf(copyright_text_2);
		assertTrue(index1 < 0);
		assertTrue(index2 >= 0);
	}
	
	public void testCopyright_with_extenders() {
		MethodPlugin plugin1 = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg1 = LibraryTestHelper.createContentPackage(plugin1, "p1");
		SupportingMaterial copyright_1 = LibraryTestHelper.createSupportingMaterial(pkg1, "copyright 1");
		copyright_1.getPresentation().setMainDescription(copyright_text_1);
		plugin1.setCopyrightStatement(copyright_1);
		
		MethodPlugin plugin2 = LibraryTestHelper.createMethodPlugin("plugin 2");
		ContentPackage pkg2 = LibraryTestHelper.createContentPackage(plugin2, "p2");
		SupportingMaterial copyright_2 = LibraryTestHelper.createSupportingMaterial(pkg2, "copyright 2");
		copyright_2.getPresentation().setMainDescription(copyright_text_2);
		plugin2.setCopyrightStatement(copyright_2);
		plugin2.getBases().add(plugin1);

		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin1);
		config.getMethodPackageSelection().add(pkg1);
		config.getMethodPluginSelection().add(plugin2);
		config.getMethodPackageSelection().add(pkg2);
		LibraryUtil.validateMethodConfiguration(config);	
		
		Task t1 = LibraryTestHelper.createTask(pkg1, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg2, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg2, "t3");

		LibraryTestHelper.setExtends(t2, t1);
		LibraryTestHelper.setExtends(t3, t2);
		
		int index1, index2, index3;
		
		String text = ConfigurationHelper.getCopyrightText(t1, config);
		index1 = text.indexOf(copyright_text_1);	
		assertTrue(index1>=0);
		
		text = ConfigurationHelper.getCopyrightText(t2, config);	
		index1 = text.indexOf(copyright_text_1);	
		index2 = text.indexOf(copyright_text_2);
		assertTrue(index1 >= 0);
		assertTrue(index2 > index1); // base copyright comes first

		String text3 = ConfigurationHelper.getCopyrightText(t3, config);	
		assertEquals(text, text3); // t2 and t3 has the same copyright, should only show once, so same as above		
	}
	
	/**
	 * R00383855 - Same copyright message gets realized twice
	 * Steps to reproduce:
		0. Create a config include plugin X and Y.
		1. Assign copyright A to plugin X.
		2. Assign copyright A to plugin Y.
		3. Have a task M in plugin Y to contribute task N in plugin X.
		4. preview task N and you will be the copyright displaed twice at the bottom.
	 *
	 */
	
	public void test_R00383855() {
		
		// same as the last part of testCopyright_with_contributors()
		testCopyright_with_contributors();
	}
}
