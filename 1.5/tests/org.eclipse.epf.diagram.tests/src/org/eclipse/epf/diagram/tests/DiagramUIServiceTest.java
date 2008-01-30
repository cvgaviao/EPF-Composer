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
package org.eclipse.epf.diagram.tests;

import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.transaction.Transaction;
import org.eclipse.epf.diagram.core.services.DiagramManager;
import org.eclipse.epf.diagram.ui.service.DiagramUIService;
import org.eclipse.epf.diagram.ui.service.IDiagramUIService;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.Task;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.gmf.runtime.notation.Node;

/**
 * JUnit tests for the org.eclipse.epf.diagram.ui.DiagramUIService classes.
 * 
 * @author Shilpa Toraskar
 * @since 1.2
 */
public class DiagramUIServiceTest extends DiagramTestCase {

	private IDiagramUIService diagramUIService = null;

	/**
	 * Creates a new instance.
	 */
	public DiagramUIServiceTest(String name) {
		super(name);
	}

	/**
	 * Tests the DiagamUIService.createDiagram method.
	 */
	public void testCreateDiagram() throws Exception {

		// create method plugin
		MethodPlugin plugin = LibraryTestHelper
				.createMethodPlugin("Test Plugin");
		ContentPackage pkg = LibraryTestHelper.createContentPackage(plugin,
				"p1");

		// create configuration
		MethodConfiguration config = LibraryTestHelper
				.createConfiguration("Test Config");

		// create process
		CapabilityPattern cp1 = LibraryTestHelper.createCapabilityPattern(
				plugin, "cp1", config);

		// update the configuration
		config.getMethodPluginSelection().add(plugin);
		config.getMethodPackageSelection().add(pkg);
		config.getMethodPackageSelection().add(
				(ProcessComponent) cp1.eContainer());

		// create task
		Task task1 = LibraryTestHelper.createTask(pkg, "Task1");
		Task task2 = LibraryTestHelper.createTask(pkg, "Task2");
		Task[] tasks = new Task[] { task1, task2 };

		Activity act1 = LibraryTestHelper.createActivity(plugin, cp1, "A1",
				tasks);
		Activity act2 = LibraryTestHelper.createActivity(plugin, cp1, "A2",
				tasks);

		diagramUIService = new DiagramUIService();
		DiagramManager mgr = DiagramManager.getInstance(cp1, this);
		Diagram diagram = null;
		try {
			Transaction tx =  mgr.getEditingDomain().startTransaction(false,
					Collections.EMPTY_MAP);
			Resource resource = mgr.getResource();
	
			diagram = diagramUIService.createActivityDiagram(cp1);
	
			Node act1Node = diagramUIService.createNode(diagram,  act1);
			Node initialNode = diagramUIService.createNode(diagram, IDiagramUIService.INITIAL_NODE, "initial");
			Node act2Node = diagramUIService.createNode(diagram, act2);
			Node decisionNode = diagramUIService.createNode(diagram, IDiagramUIService.DECISION_NODE, "decision");
			Node endNode = diagramUIService.createNode(diagram, IDiagramUIService.FINAL_NODE, "end");
			
			diagramUIService.createEdge(diagram, initialNode, act1Node, "link1");
			diagramUIService.createEdge(diagram, act1Node, decisionNode, "link2");
			diagramUIService.createEdge(diagram, decisionNode, act2Node, "link3");
			diagramUIService.createEdge(diagram, act2Node, endNode, "link4");
						
			List nodes = diagramUIService.getNodes(diagram, cp1, IDiagramUIService.ACTIVITY_NODE);
			if (!nodes.isEmpty()) {
				System.out.println("****** ACTIVITY Nodes - ");
				for (int i=0; i < nodes.size(); i++)
					System.out.println(nodes.get(i));
			}
			
			nodes = diagramUIService.getNodes(diagram, cp1, IDiagramUIService.INITIAL_NODE);
			if (!nodes.isEmpty()) {
				System.out.println("****** Initial Nodes - ");
				for (int i=0; i < nodes.size(); i++)
					System.out.println(nodes.get(i));
			}
			
			nodes = diagramUIService.getNodes(diagram, cp1, IDiagramUIService.DECISION_NODE);
			if (!nodes.isEmpty()) {
				System.out.println("****** Decision Nodes - ");
				for (int i=0; i < nodes.size(); i++)
					System.out.println(nodes.get(i));
			}
			
			nodes = diagramUIService.getAllNodes(diagram);
			if (!nodes.isEmpty()) {
				System.out.println("****** ALL Nodes - ");
				for (int i=0; i < nodes.size(); i++)
					System.out.println(nodes.get(i));
			}
			
			assertTrue(!nodes.isEmpty());
			
			List edges = diagramUIService.getEdges(diagram);
			if (!edges.isEmpty()) {
				System.out.println("****** ALL Edges - ");
				for (int i=0; i < edges.size(); i++)
					System.out.println(edges.get(i));
			}
			
			assertTrue(!edges.isEmpty());
		
			// save diagram
			resource.getContents().add(diagram.getElement());
			resource.getContents().add(diagram);
			tx.commit();
			resource.save(null);

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (mgr != null) {
				mgr.removeConsumer(this);
			}
		}		
	}

}
