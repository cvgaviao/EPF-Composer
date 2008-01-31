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

import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.util.AssociationHelper;

/**
 * Test case for incoming 0..n feature varaibility.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class Incoming0nFeatureTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public Incoming0nFeatureTest(String name) {
		super(name);
	}


	/**
	 * since 1.0m4, a work product can have multiple roles
	 *
	 */
	public void test_workProduct_ResponsibleRoles() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_ResponsibleRoles;
		List items;
		
		List list = r1.getResponsibleFor();
		list.add(a1);
		list.add(a1);
		
		// case 1
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a2);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r1));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));
		
		// case 2: 
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a2);
		LibraryTestHelper.setContributes(r2, r1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r1));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r1));
		
		// case 3: 
		// since r2 replaces r1, the end result is r2 -> a2, 
		// r1 does not exist any more, so a1 has no responsible role
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a2);
		LibraryTestHelper.setReplaces(r2, r1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(0, items.size());
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));
		
		// case 4: 
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a2);
		LibraryTestHelper.setExtends(r2, r1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(r1));
		assertTrue(items.contains(r2));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));
		
		// case 5: both r1 and r2 responsible for a1
		r1.getResponsibleFor().clear();
		r2.getResponsibleFor().clear();
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a1);
		LibraryTestHelper.setExtends(r2, r1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size()); // this need to wait till the opposite feature change from phong is delivered
		assertTrue(items.contains(r1));
		assertTrue(items.contains(r2));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(0, items.size());
		
	}
	
	public void test_workProduct_ResponsibleRoles_with_contributors() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		Role r3 = LibraryTestHelper.createRole(pkg, "r3");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_ResponsibleRoles;
		List items;
		
		// case 1
		r2.getResponsibleFor().add(a2);
		LibraryTestHelper.setContributes(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(1, items.size());
		assertTrue(items.contains(r2));
		
		// case 2: 
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a2);
		LibraryTestHelper.setContributes(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(r1));
		assertTrue(items.contains(r2));
				
	}
	
	public void test_workProduct_ResponsibleRoles_with_replacer() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
//		Role r3 = LibraryTestHelper.createRole(pkg, "r3");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
//		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_ResponsibleRoles;
		List items;
		
		// case 1
		r1.getResponsibleFor().add(a1);
		r2.getResponsibleFor().add(a2);
		LibraryTestHelper.setReplaces(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(r1));
		assertTrue(items.contains(r2));
		
	}
	
}
