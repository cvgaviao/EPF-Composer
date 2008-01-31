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
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.Domain;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.util.AssociationHelper;

/**
 * Test case for incoming 0..1 feature varaibility.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class Incoming01FeatureTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public Incoming01FeatureTest(String name) {
		super(name);
	}

	public void test_workProduct_Domain() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Domain d1 = LibraryTestHelper.createDomain(plugin, "d1");
		Domain d2 = LibraryTestHelper.createDomain(plugin, "d2");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_Domains;
		
		// since 1.2, this is a to-many feature, not to-one
		// case 1
		d1.getWorkProducts().add(a1);
		d2.getWorkProducts().add(a2);
		List items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d1, items.get(0));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d2, items.get(0));
		
		// case 2: 
		LibraryTestHelper.setContributes(d2, d1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d1, items.get(0));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d1, items.get(0));
		
		// case 3: 
		// if d2 replaces d1, it's outgoing to-many association is repalced, 
		// i.e the end result is d2 -> a2, but a1 does not have a domain
		LibraryTestHelper.setReplaces(d2, d1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(0, items.size());
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d2, items.get(0));
		
		// case 4: 
		LibraryTestHelper.setExtends(d2, d1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size());
		assertEquals(d1, items.get(0));
		assertEquals(d2, items.get(1));
		
		items = ConfigurationHelper.calc0nFeatureValue(a2, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d2, items.get(0));
	}
	
	public void test_workProduct_Domain_with_contributors() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Domain d1 = LibraryTestHelper.createDomain(plugin, "d1");
		Domain d2 = LibraryTestHelper.createDomain(plugin, "d2");
		Domain d3 = LibraryTestHelper.createDomain(plugin, "d3");
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
		Artifact a3 = LibraryTestHelper.createArtifact(pkg, "a3");
				
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config 1");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);	
		DefaultElementRealizer realizer = new DefaultElementRealizer(config);
		
		OppositeFeature ofeature = AssociationHelper.WorkProduct_Domains;
		
		// case 1
		d2.getWorkProducts().add(a2);
		LibraryTestHelper.setContributes(a2, a1);
		List items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(1, items.size());
		assertEquals(d2, items.get(0));
		
		// case 2: 
		d1.getWorkProducts().add(a1);
		d2.getWorkProducts().add(a2);
		LibraryTestHelper.setContributes(a2, a1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(d1));
		assertTrue(items.contains(d2));
				
		// case 3: more than contributors, return null
		d1.getWorkProducts().remove(a1);
		d2.getWorkProducts().add(a2);
		d3.getWorkProducts().add(a3);
		LibraryTestHelper.setContributes(a3, a1);
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size());
		assertTrue(items.contains(d2));
		assertTrue(items.contains(d3));
	}
	
}
