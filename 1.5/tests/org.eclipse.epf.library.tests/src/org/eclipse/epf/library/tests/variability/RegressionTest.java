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
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleSet;
import org.eclipse.epf.uma.Section;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.util.AssociationHelper;

/**
 * record all bugs relating to rariability so that we can do regression test on those issues.
 * 
 * @author Jinhua Xi
 * @since 1.0
 *
 */
public class RegressionTest extends VariablityBaseTestCase {

	public RegressionTest(String name) {
		super(name);
	}
	
	/**
	 * 	140875Inability to add responsible for relationship to a work product
	 * 
	 	If I make a role, R2, in a plug-in, P2, responsible for a work product, WP2,
		that contributes to a work product, WP1, in another plug-in, P1, that
		responsibility does not appear to be added.  Specifically, the fact that R2 is
		responsibile for WP1 is not displayed in the Browse perspective for a
		configuration that contains both plug-ins.  Also, the fact that R2 is
		responsible for WP2 is not displayed in the Preview tab for R2 or WP2
		
		If the base does not have an incoming relationship, then it should be able to
		be contributed.  In this example, WP1 did not have a responsible role
		relationship.
	 *
	 */
	public void test_140875() {
		// this should not be an issue any more since 1.0m4.
		// WP --> Role is to many association now.
		
		MethodPlugin plugin1 = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg1 = LibraryTestHelper.createContentPackage(plugin1, "p1");

		MethodPlugin plugin2 = LibraryTestHelper.createMethodPlugin("plugin 2");
		ContentPackage pkg2 = LibraryTestHelper.createContentPackage(plugin2, "p2");

		plugin2.getBases().add(plugin1);
		
//		Role r1 = LibraryTestHelper.createRole(pkg1, "r1");
		Artifact wp1 = LibraryTestHelper.createArtifact(pkg1, "wp1");
		
		Role r2 = LibraryTestHelper.createRole(pkg2, "r2");
		Artifact wp2 = LibraryTestHelper.createArtifact(pkg2, "wp2");
		
		r2.getResponsibleFor().add(wp2);
		LibraryTestHelper.setContributes(wp2, wp1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin1);
		config.getMethodPackageSelection().add(pkg1);
		config.getMethodPluginSelection().add(plugin2);
		config.getMethodPackageSelection().add(pkg2);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		List items;
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getRole_ResponsibleFor();
		items = ConfigurationHelper.calc0nFeatureValue(r2, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(wp1));
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_ResponsibleRoles;
		items = ConfigurationHelper.calc0nFeatureValue(wp2, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));

	}
	
	/**
	 * 143871 - An extending checklist with no items does not realize the base checklist items
	 * 
		Have a base task, Task A, with some steps.
		Extend it with a new Task B, which has no steps.
		
		Browsing Task B should show the steps of Task A, but it does not.
		If Task B is subsequently assigned some steps, then all will be realized in the
		browser.  Further, if the Task B's steps are then deleted, the realization will
		still show Task A's steps.
		
		The description applies to checklists, and should read:
		Have a base checklist, Checklist A, with some checklist items.
		Extend it with a new Checklist B, which has no items.
		
		Browsing Checklist B should show the steps of Checklist A, but it does not.
		If Checklist B is subsequently assigned some items, then all will be realized
		in the browser.  Further, if the Checklist B's items are then deleted, the
		realization will still show Checklist A's items.
	 */
	public void test_143871_task_steps() {	
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
			
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
	
		// these two should generate the same result
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_Sections();
		EStructuralFeature feature2 = UmaPackage.eINSTANCE.getTask_Steps();
		List items;
				
		Section s1_1 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s1", "t1s1");
		Section s1_2 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s2", "t1s2");
		

		LibraryTestHelper.setExtends(t2, t1);
		
		items = ConfigurationHelper.calc0nFeatureValue(t2.getPresentation(), t2, feature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));

		items = ConfigurationHelper.calc0nFeatureValue(t2, feature2, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));

		// then set the sections for task 2
		Section s2_1 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s1", "t2s1");
		Section s2_2 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s2", "t2s2");

		items = ConfigurationHelper.calc0nFeatureValue(t2.getPresentation(), t2, feature, realizer);
		assertEquals(4, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

		items = ConfigurationHelper.calc0nFeatureValue(t2, feature2, realizer);
		assertEquals(4, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

	}
	
	public void test_143871_checklist_items() {	
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Checklist c1 = LibraryTestHelper.createChecklist(pkg, "c1");
		Checklist c2 = LibraryTestHelper.createChecklist(pkg, "c2");
			
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
	
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_Sections();
		List items;
				
		Section s1_1 = LibraryTestHelper.createSection(c1.getPresentation(), "c1s1", "c1s1");
		Section s1_2 = LibraryTestHelper.createSection(c1.getPresentation(), "c1s2", "c1s2");
		
		LibraryTestHelper.setExtends(c2, c1);
		
		items = ConfigurationHelper.calc0nFeatureValue(c2.getPresentation(), c2, feature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));

		// then set the sections for checklist 2
		Section s2_1 = LibraryTestHelper.createSection(c2.getPresentation(), "c2s1", "c2s1");
		Section s2_2 = LibraryTestHelper.createSection(c2.getPresentation(), "c2s2", "c2s2");

		items = ConfigurationHelper.calc0nFeatureValue(c2.getPresentation(), c2, feature, realizer);
		assertEquals(4, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

	}
	
	/**
	 * 146153 - Variability: Incoming to-many associations are not preserved in the case of role replacement and extension
	 * 
		STEPS:
		1. create roleA and roleB. Assign them to rolesetA and rolesetB respectively
		2. set roleA to replace roleB and browse
		results=> browsing shows roleA in rolesetA only. 
		expect results=> roleA should be in rolesetA and rolesetB
		
		Similar issue when roleA is extending roleB

	 *
	 */
	public void test_146153_with_replacer() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		RoleSet rs1 = LibraryTestHelper.createRoleSet(plugin, "rs1");
		RoleSet rs2 = LibraryTestHelper.createRoleSet(plugin, "rs2");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getRoleSet_Roles();
		OppositeFeature ofeature = AssociationHelper.Role_RoleSets;
		List items;
		
		rs1.getRoles().add(r1);
		rs2.getRoles().add(r2);
		
		LibraryTestHelper.setReplaces(r2, r1);
		
		items = ConfigurationHelper.calc0nFeatureValue(rs1, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));

		items = ConfigurationHelper.calc0nFeatureValue(rs2, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));
		
		items = ConfigurationHelper.calc0nFeatureValue(r2, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(rs1));
		assertTrue(items.contains(rs2));
		
	}
	
	public void test_146153_with_contributor() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		RoleSet rs1 = LibraryTestHelper.createRoleSet(plugin, "rs1");
		RoleSet rs2 = LibraryTestHelper.createRoleSet(plugin, "rs2");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getRoleSet_Roles();
		OppositeFeature ofeature = AssociationHelper.Role_RoleSets;
		List items;
		
		rs1.getRoles().add(r1);
		rs2.getRoles().add(r2);
		
		LibraryTestHelper.setContributes(r2, r1);
		
		items = ConfigurationHelper.calc0nFeatureValue(rs1, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r1));

		items = ConfigurationHelper.calc0nFeatureValue(rs2, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r1));
		
		items = ConfigurationHelper.calc0nFeatureValue(r1, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(rs1));
		assertTrue(items.contains(rs2));
		
	}
	
	public void test_146153_with_extender() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		RoleSet rs1 = LibraryTestHelper.createRoleSet(plugin, "rs1");
		RoleSet rs2 = LibraryTestHelper.createRoleSet(plugin, "rs2");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getRoleSet_Roles();
		OppositeFeature ofeature = AssociationHelper.Role_RoleSets;
		List items;
		
		rs1.getRoles().add(r1);
		rs2.getRoles().add(r2);
		
		LibraryTestHelper.setExtends(r2, r1);
		
		items = ConfigurationHelper.calc0nFeatureValue(rs1, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r1));

		items = ConfigurationHelper.calc0nFeatureValue(rs2, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));
		
		items = ConfigurationHelper.calc0nFeatureValue(r1, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(rs1));
		
		items = ConfigurationHelper.calc0nFeatureValue(r2, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(rs2));
	}
	
	/**
	 * R00382120 - Authoring:  Extending a base that has been replaced, does not extend the replacement
	 * Let A, B, and C be elements or categories.
		1.  Let A extend B.
		2.  Let C replace B.          (order does not matter)
		
		Result:
		
		A extends the associations of B, even though B is being replaced.
		
		Should be:
		
		A extends the associations of the replacement, C.
	 *
	 */
	public void test_R00382120_task_MandatoryInput_with_replacers() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task tA = LibraryTestHelper.createTask(pkg, "t1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		tA.getMandatoryInput().add(a1);
		tA.getMandatoryInput().add(a2);
		
		Task tB = LibraryTestHelper.createTask(pkg, "t2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
		Artifact a4 = LibraryTestHelper.createArtifact(pkg, "a4");	
		tB.getMandatoryInput().add(a3);
		tB.getMandatoryInput().add(a4);
	
		Task tC = LibraryTestHelper.createTask(pkg, "t3");
		Artifact a5 = LibraryTestHelper.createArtifact(pkg, "a5");
		tC.getMandatoryInput().add(a5);

		LibraryTestHelper.setExtends(tA, tB);
		LibraryTestHelper.setReplaces(tC, tB);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_MandatoryInput();
		List items = ConfigurationHelper.calc0nFeatureValue(tA, feature, realizer);
		assertEquals(3, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a5));

	}
	
	/**
	 * R00382327 - replaced element does not show incoming relationships
	 * e.g. replace a work product: the published page does not show the Tasks 
	 * the workproduct is input/output to.  
	 * It should list all the Tasks from the replaced element.
	 *
	 */
	public void test_R00382327_task_MandatoryInput_with_replacers() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");		
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		
		// t1, t3 -> a1, t2->a2, a2 -replace->a1
		// a1's tasks should be t1, t2,t3
		t1.getMandatoryInput().add(a1);
		t3.getMandatoryInput().add(a1);
		t2.getMandatoryInput().add(a2);
		
		LibraryTestHelper.setReplaces(a2, a1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_MandatoryInputTo_Tasks;
		List items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(3, items.size());		
		assertTrue(items.contains(t1));
		assertTrue(items.contains(t2));
		assertTrue(items.contains(t3));

	}
	
	/**
	 * R00382972 - Authoring: When sub-artifact extends parent artifact,
	 * that sub artifact has infinite loop in configuration explorer tree.
	 */
	public void test_R00382972_artifact_SubArtifacts() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		
		// a1 contains a2, a2 extends a1
		// what's a1's childres? a2
		// what's a2's children? a2? NOOOO!
		
		a1.getContainedArtifacts().add(a2);
		LibraryTestHelper.setExtends(a2, a1);
			
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getArtifact_ContainedArtifacts();
		List items = ConfigurationHelper.calc0nFeatureValue(a1, feature, realizer);
		assertEquals(1, items.size());		
		assertTrue(items.contains(a2));
		
		// a2's contained arfifacts should contain the ones from the base, which include itself
		// should not contain itself
		items = ConfigurationHelper.calc0nFeatureValue(a2, feature, realizer);
		assertEquals(0, items.size());		
		
	}
	
	/** 
	 * R00383528 - Browsing:  A replacing role does not pick up the task(s) for which the replaced role was primary performer
	 * 
	 *	Let Role A be primary performer for Task A,
   		Role B be primary performer for Task B.

		Now have Role B replace Role A.
		
		Browse Role B and should expect it to be the primary performer for Task A as well as Task B.
		It only shows Task B.
		
		Browse Task A and it correctly shows that Role B is the primary performer.
	 */
	public void R00383528_role_tasks_with_repalcer() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task tA = LibraryTestHelper.createTask(pkg, "t1");
		Role rA = LibraryTestHelper.createRole(pkg, "r1");
		Task tB = LibraryTestHelper.createTask(pkg, "t2");
		Role rB = LibraryTestHelper.createRole(pkg, "r2");
		
		tA.getPerformedBy().add(rA);
		tB.getPerformedBy().add(rB);
		
		LibraryTestHelper.setReplaces(rB, rA);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
			
		// outgoing feature
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_PerformedBy();
		MethodElement r = ConfigurationHelper.calc01FeatureValue(tA, feature, realizer);
		assertEquals(rB, r);	
		
		r = ConfigurationHelper.calc01FeatureValue(tB, feature, realizer);
		assertEquals(rB, r);	
		
		// incoming feature
		OppositeFeature ofeature = AssociationHelper.Role_Primary_Tasks;
		List items = ConfigurationHelper.calc0nFeatureValue(rB, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(tA));
		assertTrue(items.contains(tB));

	}
	
	/**
	 * 146153 - Variability: Incoming to-many associations are not preserved in the case of role replacement
	 * 
	 * STEPS:
		1. create roleA and roleB. Assign them to rolesetA and rolesetB respectively
		2. set roleA to replace roleB and browse
		results=> browsing shows roleA in rolesetA only. 
		expect results=> roleA should be in rolesetA and rolesetB
	 *
	 */
	public void test_146153_role_roleset_with_replacer() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		RoleSet rs1 = LibraryTestHelper.createRoleSet(plugin, "rs1");
		RoleSet rs2 = LibraryTestHelper.createRoleSet(plugin, "rs2");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		OppositeFeature ofeature = AssociationHelper.Role_RoleSets;
		List items;
		
		// case 1
		rs1.getRoles().add(r1);
		rs2.getRoles().add(r2);
		LibraryTestHelper.setReplaces(r2, r1);
		items = ConfigurationHelper.calc0nFeatureValue(r2, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(rs1));
		assertTrue(items.contains(rs2));
		
	}
	
	/**
	 * 152230 - Browsing: Role<-->WP relationship shows inconsistancy
	 * 	1. create role1 responsible for wp1
		2. create role2 responsible for wp2
		3. has wp2 replacs wp1, and wp1 contribute to wp
		4. browsing shows that
		
		in wp, it shows that both role1 and role2 are responsible
		in role1, the relationship shows it is responsible for wp
		but in role2, the relationship shows no wp
		
		To be consistant, wp should be showing in role2's relationship diagram.
	 */
	public void test_152230_role_wp() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		Artifact wp1 = LibraryTestHelper.createArtifact(pkg, "wp1");
		Artifact wp2 = LibraryTestHelper.createArtifact(pkg, "wp2");
		Artifact wp = LibraryTestHelper.createArtifact(pkg, "wp");
				
		r1.getResponsibleFor().add(wp1);
		r2.getResponsibleFor().add(wp2);
		LibraryTestHelper.setContributes(wp1, wp);
		LibraryTestHelper.setReplaces(wp2, wp1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getRole_ResponsibleFor();
		OppositeFeature ofeature = AssociationHelper.WorkProduct_ResponsibleRoles;
		List items;
		
		// both r1 and r2 responsible for wp
		items = ConfigurationHelper.calc0nFeatureValue(r1, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(wp));
		
		items = ConfigurationHelper.calc0nFeatureValue(r2, feature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(wp));
		
		items = ConfigurationHelper.calc0nFeatureValue(wp, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(r1));
		assertTrue(items.contains(r2));
		
	}
	
}
