//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
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
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.util.AssociationHelper;

/**
 * Test case for fulfillment realization
 * 
 * @author Weiping Lu
 * @since 1.5
 */
public class FulfillmentTest extends VariablityBaseTestCase {

	/**
	 * @param name
	 */
	public FulfillmentTest(String name) {
		super(name);
	}

	public void test01() {
		MethodPlugin plugin = LibraryTestHelper.createMethodPlugin("plugin 1");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin, "p1");
		Role r1 = LibraryTestHelper.createRole(pkg, "r1");
		Role r2 = LibraryTestHelper.createRole(pkg, "r2");
		
		Task t1 = LibraryTestHelper.createTask(pkg, "t1");
		Task t2 = LibraryTestHelper.createTask(pkg, "t2");
		
		Artifact slot1 = LibraryTestHelper.createArtifact(pkg, "slot1");
		Artifact slot2 = LibraryTestHelper.createArtifact(pkg, "slot2");
		slot1.setIsAbstract(true);
		slot2.setIsAbstract(true);
		slot1.setPresentationName("b_slot");
		slot2.setPresentationName("a_slot");
		
		Artifact a1 = LibraryTestHelper.createArtifact(pkg, "a1");
		a1.getFulfills().add(slot1);
		a1.getFulfills().add(slot2);
		
		Artifact a2 = LibraryTestHelper.createArtifact(pkg, "a2");
		a2.getFulfills().add(slot1);
		a2.getFulfills().add(slot2);
		
		a1.setPresentationName("b_a1");
		a2.setPresentationName("a_a2");
		
		MethodConfiguration config = LibraryTestHelper.createConfiguration("config");
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		LibraryUtil.validateMethodConfiguration(config);
		
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(config);
		
		EStructuralFeature feature;
		OppositeFeature ofeature;
		List items;
		
		r1.getResponsibleFor().add(slot1);
		r2.getResponsibleFor().add(slot2);
		
		t1.getPerformedBy().add(r1);
		t2.getPerformedBy().add(r2);
		
		ofeature = AssociationHelper.WorkProduct_ResponsibleRoles;
		items = ConfigurationHelper.calc0nFeatureValue(a1, ofeature, realizer);
		assertEquals(2, items.size());
		
		items = ConfigurationHelper.calcFulfillableElement_Fulfills(a1, realizer);
		assertEquals(slot2, items.get(0));
		assertEquals(slot1, items.get(1));
		
		ofeature = AssociationHelper.FulFills_FullFillableElements;
		items = ConfigurationHelper.calcFulfills_FulfillableElement(slot1, config);
		assertEquals(2, items.size());
		assertEquals(a2, items.get(0));
		assertEquals(a1, items.get(1));
	}
}
