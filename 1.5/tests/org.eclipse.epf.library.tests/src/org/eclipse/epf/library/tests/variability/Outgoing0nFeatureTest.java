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
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Section;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;

/**
 * Test case for 0..n feature varaibility.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class Outgoing0nFeatureTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public Outgoing0nFeatureTest(String name) {
		super(name);
	}

	public void test_task_MandatoryInput() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
		
		t1.getMandatoryInput().add(a1);
		t1.getMandatoryInput().add(a2);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_MandatoryInput();
		List items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		
		// how about a2 contributes to a1
		LibraryTestHelper.setContributes(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(1, items.size());		
		assertTrue(items.contains(a1));
		
		// how about a2 replaces a1
		LibraryTestHelper.setReplaces(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(1, items.size());		
		assertTrue(items.contains(a2));
		
		// how about a2 extends a1
		LibraryTestHelper.setExtends(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
	}
	
	public void test_task_MandatoryInput_with_contributor() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		t1.getMandatoryInput().add(a1);
		t1.getMandatoryInput().add(a2);
		
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
		Artifact a4 = LibraryTestHelper.createArtifact(pkg, "a4");	
		t2.getMandatoryInput().add(a3);
		t2.getMandatoryInput().add(a4);
		
		LibraryTestHelper.setContributes(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_MandatoryInput();
		List items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(4, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		
		// how about a2 contributes to a1
		LibraryTestHelper.setContributes(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(3, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		
		// how about a3 replaces a1
		LibraryTestHelper.setReplaces(a3, a1);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		
		// how about a4 replaces a1 too: a3 and a3 will be ignored, a2 is resulting to a1, so only get a1
		LibraryTestHelper.setReplaces(a4, a1);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(1, items.size());		
		assertTrue(items.contains(a1));
	}
	
	
	public void test_task_MandatoryInput_with_contributors() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		t1.getMandatoryInput().add(a1);
		t1.getMandatoryInput().add(a2);
		
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
		Artifact a4 = LibraryTestHelper.createArtifact(pkg, "a4");	
		t2.getMandatoryInput().add(a3);
		t2.getMandatoryInput().add(a4);

		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
		Artifact a5 = LibraryTestHelper.createArtifact(pkg, "a5");
		t3.getMandatoryInput().add(a5);
	
		LibraryTestHelper.setContributes(t2, t1);
		LibraryTestHelper.setContributes(t3, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_MandatoryInput();
		List items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(5, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		assertTrue(items.contains(a5));
		
		// how about t3 contributes to t2, same result
		LibraryTestHelper.setContributes(t3, t2);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(5, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		assertTrue(items.contains(a5));
		
	}
	
	
	public void test_task_MandatoryInput_with_replacers() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		t1.getMandatoryInput().add(a1);
		t1.getMandatoryInput().add(a2);
		
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
		Artifact a4 = LibraryTestHelper.createArtifact(pkg, "a4");	
		t2.getMandatoryInput().add(a3);
		t2.getMandatoryInput().add(a4);
	
		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
		Artifact a5 = LibraryTestHelper.createArtifact(pkg, "a5");
		t3.getMandatoryInput().add(a5);

		LibraryTestHelper.setReplaces(t2, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		MethodElement t = ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t2, t);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_MandatoryInput();
		List items = ConfigurationHelper.calc0nFeatureValue(t, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		
		// how about t3 replaces to t2
		LibraryTestHelper.setReplaces(t3, t2);
		
		t = ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t3, t);
		
		items = ConfigurationHelper.calc0nFeatureValue(t, feature, realizer);
		assertEquals(1, items.size());		
		assertTrue(items.contains(a5));

		// how about t3 replaces to t1, so t2 and t3 both replaces t1, th ereplacer is ignored
		LibraryTestHelper.setReplaces(t3, t1);
		
		t = ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t1, t);
		
		items = ConfigurationHelper.calc0nFeatureValue(t, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));

	}
	
	public void test_task_MandatoryInput_with_extenders() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");	
		t1.getMandatoryInput().add(a1);
		t1.getMandatoryInput().add(a2);
		
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
		Artifact a4 = LibraryTestHelper.createArtifact(pkg, "a4");	
		t2.getMandatoryInput().add(a3);
		t2.getMandatoryInput().add(a4);

		Task t3 = LibraryTestHelper.createTask(pkg, "t3");
		Artifact a5 = LibraryTestHelper.createArtifact(pkg, "a5");
		t3.getMandatoryInput().add(a5);
	
		LibraryTestHelper.setExtends(t2, t1);
		LibraryTestHelper.setExtends(t3, t1);
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTask_MandatoryInput();
		List items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));

		items = ConfigurationHelper.calc0nFeatureValue(t2, feature, realizer);
		assertEquals(4, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		
		items = ConfigurationHelper.calc0nFeatureValue(t3, feature, realizer);
		assertEquals(3, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a5));
		
		// how about t3 extends t2
		LibraryTestHelper.setExtends(t3, t2);
		items = ConfigurationHelper.calc0nFeatureValue(t1, feature, realizer);
		assertEquals(2, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		
		items = ConfigurationHelper.calc0nFeatureValue(t2, feature, realizer);
		assertEquals(4, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		
		items = ConfigurationHelper.calc0nFeatureValue(t3, feature, realizer);
		assertEquals(5, items.size());		
		assertTrue(items.contains(a1));
		assertTrue(items.contains(a2));
		assertTrue(items.contains(a3));
		assertTrue(items.contains(a4));
		assertTrue(items.contains(a5));

	}
	
	// test the sections
	public void test_task_steps_with_contributors() {
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
	
		// these two should generate the same result
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_Sections();
		EStructuralFeature feature2 = UmaPackage.eINSTANCE.getTask_Steps();
		List items;
				
		Section s1_1 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s1", "t1s1");
		Section s1_2 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s2", "t1s2");

		Section s2_1 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s1", "t2s1");
		Section s2_2 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s2", "t2s2");

		Section s3_1 = LibraryTestHelper.createSection(t3.getPresentation(), "t3s1", "t3s1");
		Section s3_2 = LibraryTestHelper.createSection(t3.getPresentation(), "t3s2", "t3s2");
		
		// case 1
		items = ConfigurationHelper.calc0nFeatureValue(t1.getPresentation(), t1, feature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));

		items = ConfigurationHelper.calc0nFeatureValue(t1, feature2, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));

		// case 2
		LibraryTestHelper.setContributes(t2, t1);
		items = ConfigurationHelper.calc0nFeatureValue(t1.getPresentation(), t1, feature, realizer);
		assertEquals(4, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

		items = ConfigurationHelper.calc0nFeatureValue(t1, feature2, realizer);
		assertEquals(4, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

	}
	
	// test the sections for replacers
	public void test_task_steps_with_replacers() {
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
	
		// these two should generate the same result
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_Sections();
		EStructuralFeature feature2 = UmaPackage.eINSTANCE.getTask_Steps();
		List items;
		Task t;
		
		Section s1_1 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s1", "t1s1");
		Section s1_2 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s2", "t1s2");

		Section s2_1 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s1", "t2s1");
		Section s2_2 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s2", "t2s2");

		Section s3_1 = LibraryTestHelper.createSection(t3.getPresentation(), "t3s1", "t3s1");
		Section s3_2 = LibraryTestHelper.createSection(t3.getPresentation(), "t3s2", "t3s2");
		

		LibraryTestHelper.setReplaces(t2, t1);
		t = (Task)ConfigurationHelper.getCalculatedElement(t1, realizer);
		assertEquals(t2, t);  

		items = ConfigurationHelper.calc0nFeatureValue(t.getPresentation(), t, feature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

		items = ConfigurationHelper.calc0nFeatureValue(t, feature2, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s2_1));
		assertTrue(items.contains(s2_2));

	}
	
	// test the sections
	public void test_task_steps_with_extenders() {
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
	
		// these two should generate the same result
		EStructuralFeature feature = UmaPackage.eINSTANCE.getContentDescription_Sections();
		EStructuralFeature feature2 = UmaPackage.eINSTANCE.getTask_Steps();
		List items;
				
		Section s1_1 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s1", "t1s1");
		Section s1_2 = LibraryTestHelper.createSection(t1.getPresentation(), "t1s2", "t1s2");

		Section s2_1 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s1", "t2s1");
		Section s2_2 = LibraryTestHelper.createSection(t2.getPresentation(), "t2s2", "t2s2");

//		Section s3_1 = LibraryTestHelper.createSection(t3.getPresentation(), "t3s1", "t3s1");
//		Section s3_2 = LibraryTestHelper.createSection(t3.getPresentation(), "t3s2", "t3s2");
		
		LibraryTestHelper.setExtends(t2, t1);
		LibraryTestHelper.setExtends(t3, t1);

		// case 1
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

		// case 2, t3 has no steps, this was once a bug since the contentDescription object is not created
		// and the realization failed
		items = ConfigurationHelper.calc0nFeatureValue(t3.getPresentation(), t3, feature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));

		items = ConfigurationHelper.calc0nFeatureValue(t3, feature2, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(s1_1));
		assertTrue(items.contains(s1_2));
	}
	
}
