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

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;

/**
 * Test case for attribute feature varaibility.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class AttributeFeatureTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public AttributeFeatureTest(String name) {
		super(name);
	}

	public void test_task_mainDescription_with_contributors() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_MainDescription();
		String text;
		
		final String t1text = "--t1 text--";
		final String t2text = "--t2 text--";
		final String t3text = "--t3 text--";
		
		t1.getPresentation().setMainDescription(t1text);
		t2.getPresentation().setMainDescription(t2text);
		t3.getPresentation().setMainDescription(t3text);
		
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t1.getPresentation(), t1, feature, config);
		assertEquals(t1text, text);
		
		LibraryTestHelper.setContributes(t2, t1);
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t1.getPresentation(), t1, feature, config);
		assertTrue(text != null 
				&& text.indexOf(t1text) >= 0 
				&& text.indexOf(t2text) >= 0 );
		
		LibraryTestHelper.setContributes(t3, t1);
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t1.getPresentation(), t1, feature, config);
		assertTrue(text != null 
				&& text.indexOf(t1text) >= 0 
				&& text.indexOf(t2text) >= 0 
				&& text.indexOf(t3text) >= 0 );

	}
	
	public void test_task_mainDescription_with_replacers() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);

		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_MainDescription();
		String text;
		Task t;
		
		final String t1text = "--t1 text--";
		final String t2text = "--t2 text--";
		final String t3text = "--t3 text--";
		
		t1.getPresentation().setMainDescription(t1text);
		t2.getPresentation().setMainDescription(t2text);
		t3.getPresentation().setMainDescription(t3text);
		
		// 1
		LibraryTestHelper.setReplaces(t2, t1);
		t = (Task)ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t2, t);  // if this fail, the remaining code will not be executed

		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t.getPresentation(), t, feature, config);
		assertEquals(t2text, text);
		
		// 2
		LibraryTestHelper.setReplaces(t3, t2);
		t = (Task)ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t3, t);  // if this fail, the remaining code will not be executed

		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t.getPresentation(), t, feature, config);
		assertEquals(t3text, text);

		// 3
		LibraryTestHelper.setReplaces(t3, t1);
		t = (Task)ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t1, t);  // if this fail, the remaining code will not be executed

		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t.getPresentation(), t, feature, config);
		assertEquals(t1text, text);

	}
	
	
	public void test_task_mainDescription_with_extenders() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_MainDescription();
		String text;
		
		final String t1text = "--t1 text--";
		final String t2text = "--t2 text--";
		final String t3text = "--t3 text--";
		
		t1.getPresentation().setMainDescription(t1text);
		t3.getPresentation().setMainDescription(t3text);
	
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t2.getPresentation(), t2, feature, config);
		assertTrue(text==null || text.equals(""));
		
		LibraryTestHelper.setExtends(t2, t1);
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t2.getPresentation(), t2, feature, config);
		assertEquals(t1text, text);
		
		LibraryTestHelper.setExtends(t3, t1);
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t3.getPresentation(), t3, feature, config);
		assertEquals(t3text, text);

	}
	
	public void test_task_mainDescription_with_replacer_contributor_extender() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
		Task t4 = LibraryTestHelper.createTask(pkg, "t4");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);

		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_MainDescription();
		String text;
		Task t;
		
		final String t1text = "--t1 text--";
		final String t2text = "--t2 text--";
		final String t3text = "--t3 text--";
		final String t4text = "--t4 text--";
		
		t1.getPresentation().setMainDescription(t1text);
		t2.getPresentation().setMainDescription(t2text);
		t3.getPresentation().setMainDescription(t3text);
		//t4.getPresentation().setMainDescription(t4text);
		
		// 1
		LibraryTestHelper.setReplaces(t2, t1);
		LibraryTestHelper.setContributes(t3, t1);
		LibraryTestHelper.setExtends(t4, t1);

		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t4.getPresentation(), t4, feature, config);
		assertEquals(t2text, text);
		
		// 2
		LibraryTestHelper.setContributes(t3, t2);
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t4.getPresentation(), t4, feature, config);
		assertTrue(text != null 
				&& text.indexOf(t2text) >= 0 
				&& text.indexOf(t3text) >= 0 );

		// 3 since t3 contributes to t4, the attribute value from base is not inherited.
		LibraryTestHelper.setContributes(t3, t4);
		text = (String)ConfigurationHelper.calcAttributeFeatureValue(t4.getPresentation(), t4, feature, config);
		assertTrue(text != null 
				&& text.indexOf(t3text) >= 0 );

	}
	
}
