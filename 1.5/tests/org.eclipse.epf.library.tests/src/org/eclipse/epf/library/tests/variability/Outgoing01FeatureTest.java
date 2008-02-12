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

import java.util.List;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;

/**
 * Test case for 0..1 feature varaibility.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class Outgoing01FeatureTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public Outgoing01FeatureTest(String name) {
		super(name);
	}


	public void test_task_performedBy() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		
		t1.getPerformedBy().add(r1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		//MethodElement r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		List<MethodElement> list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r1, list.get(0));
	}
	
	public void test_task_performedBy_with_contributor() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		
		t1.getPerformedBy().add(r1);
		t2.getPerformedBy().add(r2);
		
		// t2 contributes to t1, since t1 already has a to-one association, 
		// the one from the contributor is ignored
		LibraryTestHelper.setContributes(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		//MethodElement r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		//assertEquals(r1, r);
		List<MethodElement> list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r1, list.get(0));
	}
	
	public void test_task_performedBy_from_contributor() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
//		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		
		// don't set performer in t1, so it will get from the contributor
		//t1.setPerformedBy(r1);
		t2.getPerformedBy().add(r2);
		
		// t2 contributes to t1, since t1 does not have a to-one association, 
		// the one from the contributor is used
		LibraryTestHelper.setContributes(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		//MethodElement r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		//assertEquals(r2, r);
		List<MethodElement> list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r2, list.get(0));
	}
	
	public void test_task_performedBy_with_contributors() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
//		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");

		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
		Role r3 = LibraryTestHelper.createRole(pkg, "r3");

		// don't set performer in t1, so it will get from the contributor
		t2.getPerformedBy().add(r2);
		t3.getPerformedBy().add(r3);
		
		// t2 and t3 both contribute to t1, even though t1 does not have a to-one association, 
		// the ones from the contributors are ignored since there are more than one
		LibraryTestHelper.setContributes(t2, t1);
		LibraryTestHelper.setContributes(t3, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		MethodElement r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		assertEquals(null, r);
	}
	
	public void test_task_performedBy_with_replacer() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		
		t1.getPerformedBy().add(r1);
		t2.getPerformedBy().add(r2);
		
		LibraryTestHelper.setReplaces(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		MethodElement t = ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t2, t);  // if this fail, the remaining code will not be executed
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		//MethodElement r = ConfigurationHelper.calc01FeatureValue(t, feature, realizer);
		//assertEquals(r2, r);
		List<MethodElement> list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r1, list.get(0));
	}
	
	public void test_task_performedBy_with_replacer_no_performer() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		
		t1.getPerformedBy().add(r1);
		
		// t2 has no primary performer
//		t2.setPerformedBy(r2);
		
		LibraryTestHelper.setReplaces(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		MethodElement t = ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t2, t);  // if this fail, the remaining code will not be executed
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		MethodElement r = ConfigurationHelper.calc01FeatureValue(t, feature, realizer);
		
		if (ElementRealizer.isExtendReplaceEnabled() ) {
			assertEquals(r1, r);  
		} else {
			assertEquals(null, r);
		}
	}
	
	public void test_task_performedBy_with_replacers() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
		Role r3 = LibraryTestHelper.createRole(pkg, "r3");
		
		t1.getPerformedBy().add(r1);
		t2.getPerformedBy().add(r2);
		t3.getPerformedBy().add(r3);
		
		LibraryTestHelper.setReplaces(t2, t1);
		LibraryTestHelper.setReplaces(t3, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		// if more than one replacer, all replacers are ignored
		MethodElement t = ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t1, t); 
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		//MethodElement r = ConfigurationHelper.calc01FeatureValue(t, feature, realizer);
		//assertEquals(r1, r);
		List<MethodElement> list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r1, list.get(0));
	}
	
	
	public void test_task_performedBy_with_extenders() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
//		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
//		Role r3 = LibraryTestHelper.createRole(pkg, "r3");
		
		LibraryTestHelper.setExtends(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		MethodElement r;
		
		// case 1:
		//t1.getPerformedBy().add(null);
		t2.getPerformedBy().add(r2);		
		//r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		//assertEquals(null, r);
		List<MethodElement> list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertTrue(list.isEmpty());
		
		//r = ConfigurationHelper.calc01FeatureValue(t2, feature, realizer);
		//assertEquals(r2, r);
		list = ConfigurationHelper.calc0nFeatureValue(t2, feature, realizer);
		assertEquals(r2, list.get(0));
		
		// case 2:
		t1.getPerformedBy().add(r1);
		t2.getPerformedBy().add(r1);

		//r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		//assertEquals(r1, r);
		list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r1, list.get(0));
		
		//r = ConfigurationHelper.calc01FeatureValue(t2, feature, realizer);
		//assertEquals(r2, r);
		list = ConfigurationHelper.calc0nFeatureValue(t2, feature, realizer);
		assertTrue(list.size() == 2);
		assertEquals(r2, list.get(0));
		assertEquals(r1, list.get(1));
		
		// case 3:
		t1.getPerformedBy().clear();
		t2.getPerformedBy().clear();
		t1.getPerformedBy().add(r1);
		//t2.getPerformedBy().add(null);

		//r = ConfigurationHelper.calc01FeatureValue(t1, feature, realizer);
		//assertEquals(r1, r);
		list = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(r1, list.get(0));
		
		//r = ConfigurationHelper.calc01FeatureValue(t2, feature, realizer);
		//assertEquals(r1, r);
		list = ConfigurationHelper.calc0nFeatureValue(t2, feature, realizer);
		assertEquals(r1, list.get(0));
		
	}
}
