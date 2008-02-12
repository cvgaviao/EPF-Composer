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
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;

/**
 * Test case for activity varaibility.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class ActivityVariabilityTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public ActivityVariabilityTest(String name) {
		super(name);
	}
	
	public void test_cp_contribution_for_attributes() {
		
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");

		// now create a capability pattern
		CapabilityPattern cp1 = LibraryTestHelper.createCapabilityPattern(plugin, "CP1", config);
		
		// now create a contributing capability pattern
		CapabilityPattern cp2 = LibraryTestHelper.createCapabilityPattern(plugin, "CP2", config);
		
		LibraryTestHelper.setContributes(cp2, cp1);
		
		// update the configuration
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		config.getMethodPackageSelection().add((ProcessComponent)cp1.eContainer());
		config.getMethodPackageSelection().add((ProcessComponent)cp2.eContainer());
		
		LibraryUtil.validateMethodConfiguration(config);	
		
		final String cp1_text = "cp1 main desc";
		final String cp2_text = "cp2 main desc";
			
		cp1.getPresentation().setMainDescription(cp1_text);
		cp2.getPresentation().setMainDescription(cp2_text);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_MainDescription();
		String  text = (String)ConfigurationHelper.calcAttributeFeatureValue(cp1.getPresentation(), cp1, feature, config);
		int i1 = text.indexOf(cp1_text);
		int i2 = text.indexOf(cp2_text);
		assertTrue(i1 >= 0 );
		assertTrue(i2 > i1);
	}
	
	public void test_cp_contribution_for_guidances() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Checklist ck1 = LibraryTestHelper.createChecklist(pkg, "ck1");
		Checklist ck2 = LibraryTestHelper.createChecklist(pkg, "ck2");

		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");

		// now create a capability pattern
		CapabilityPattern cp1 = LibraryTestHelper.createCapabilityPattern(plugin, "CP1", config);
		cp1.getChecklists().add(ck1);
		
		// now create a contributing capability pattern
		CapabilityPattern cp2 = LibraryTestHelper.createCapabilityPattern(plugin, "CP2", config);
		cp2.getChecklists().add(ck2);
		
		LibraryTestHelper.setContributes(cp2, cp1);
		
		// update the configuration
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		config.getMethodPackageSelection().add((ProcessComponent)cp1.eContainer());
		config.getMethodPackageSelection().add((ProcessComponent)cp2.eContainer());
		
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getBreakdownElement_Checklists();
		List items = ConfigurationHelper.calc0nFeatureValue(cp1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(ck1));
		assertTrue(items.contains(ck2));
	}
	
	public void test_cp_contribution_for_breakdownElements() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");

		// now create a capability pattern
		CapabilityPattern cp1 = LibraryTestHelper.createCapabilityPattern(plugin, "CP1", config);
		Activity act1 = LibraryTestHelper.createActivity(plugin, cp1, "act1", null);
		
		// now create a contributing capability pattern
		CapabilityPattern cp2 = LibraryTestHelper.createCapabilityPattern(plugin, "CP2", config);
		Activity act2 = LibraryTestHelper.createActivity(plugin, cp2, "act2", null);
		
		LibraryTestHelper.setContributes(cp2, cp1);
		
		// update the configuration
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		config.getMethodPackageSelection().add((ProcessComponent)cp1.eContainer());
		config.getMethodPackageSelection().add((ProcessComponent)cp2.eContainer());
		
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getActivity_BreakdownElements();
		List items = ConfigurationHelper.calc0nFeatureValue(cp1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(act1));
		assertTrue(items.contains(act2));
	}
	
	
	public void test_activity_breakdownElements_with_contributor() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		
		// first set of task, role, wps
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Role r1 = LibraryTestHelper.createRole(pkg, "t1");
		Artifact a1_1 = LibraryTestHelper.createArtifact(pkg, "a11");
		Artifact a1_2 = LibraryTestHelper.createArtifact(pkg, "a12");
		t1.getPerformedBy().add(r1);
		t1.getMandatoryInput().add(a1_1);
		t1.getMandatoryInput().add(a1_2);
		t1.getOutput().add(a1_2);
		r1.getResponsibleFor().add(a1_1);
		
		// second set of task, role, wps
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Role r2 = LibraryTestHelper.createRole(pkg, "t2");
		Artifact a2_1 = LibraryTestHelper.createArtifact(pkg, "a21");
		Artifact a2_2 = LibraryTestHelper.createArtifact(pkg, "a22");
		t2.getPerformedBy().add(r2);
		t2.getMandatoryInput().add(a2_1);
		t2.getMandatoryInput().add(a2_2);
		t2.getOutput().add(a2_2);
		r2.getResponsibleFor().add(a2_1);

		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");

		// now create a capability pattern
		CapabilityPattern cp1 = LibraryTestHelper.createCapabilityPattern(plugin, "CP1", config);
		Activity act1 = LibraryTestHelper.createActivity(plugin, cp1, "act1", new Task[]{t1});
		
		// now create a contributing capability pattern
		CapabilityPattern cp2 = LibraryTestHelper.createCapabilityPattern(plugin, "CP2", config);
		Activity act2 = LibraryTestHelper.createActivity(plugin, cp1, "act2", new Task[]{t2});
		
		LibraryTestHelper.setContributes(cp2, cp1);
		
		// update the configuration
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		config.getMethodPackageSelection().add((ProcessComponent)cp1.eContainer());
		config.getMethodPackageSelection().add((ProcessComponent)cp2.eContainer());
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getActivity_BreakdownElements();
		List items = ConfigurationHelper.calc0nFeatureValue(cp1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(act1));
		assertTrue(items.contains(act2));
	}
}
