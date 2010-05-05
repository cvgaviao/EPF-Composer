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
package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;


/**
 * Helper class that manages the use of Constraint in library to store
 * implementation-specific data
 * 
 * @author Phong Nguyen Le - Dec 1, 2005
 * @author Weiping Lu, Aripl 20, 2010
 * @since 1.0
 */
public final class ConstraintManager {
	
	/** Constants for constraint name */
	public static final String ACITIVY_DIAGRAM = "diagram"; //$NON-NLS-1$

	public static final String PROCESS_SUPPRESSION = ""; //$NON-NLS-1$
	
	//Owner: work-product, body: state name
	public static final String WORKPRODUCT_State = "state"; //$NON-NLS-1$
	
	//Owner: task-descriptor, body: guid of work-product descriptor
	public static final String TASKDESCRIPTOR_WpStates = "wpStates"; //$NON-NLS-1$	

	public static final Constraint getConstraint(MethodElement e,
			String constraintName, boolean create) {
		if (constraintName == null) {
			return null;
		}
		Constraint constraint = null;
		for (Iterator iter = e.getOwnedRules().iterator(); iter.hasNext();) {
			Constraint c = (Constraint) iter.next();
			if (c.getName().equals(constraintName)) {
				constraint = c;
				break;
			}
		}
		if (constraint == null && create) {
			constraint = UmaFactory.eINSTANCE.createConstraint();
			constraint.setName(constraintName);
			e.getOwnedRules().add(constraint);
		}
		return constraint;
	}
	
	public static final Constraint getWorkProductState(WorkProduct wp,
			String stateName, boolean create, IActionManager actionManager) {
		if (wp == null || stateName == null || stateName.trim().length() == 0) {
			return null;
		}

		for (Iterator iter = wp.getOwnedRules().iterator(); iter.hasNext();) {
			Constraint constraint = (Constraint) iter.next();
			if (constraint.getName().equals(WORKPRODUCT_State)
					&& constraint.getBody().equals(stateName)) {
				return constraint;
			}
		}

		if (create) {
			return createConstraint(wp, WORKPRODUCT_State, stateName, actionManager);
		}

		return null;
	}

	private static Constraint createConstraint(MethodElement owner, String name,
			String body, IActionManager actionManager) {
		Constraint constraint = UmaFactory.eINSTANCE.createConstraint();
		if (actionManager == null) {
			constraint.setName(name);
			constraint.setBody(body);
			owner.getOwnedRules().add(constraint);
		} else {
			UmaPackage up = UmaPackage.eINSTANCE;
			actionManager.doAction(IActionManager.SET, constraint, up
					.getNamedElement_Name(), name, -1);
			actionManager.doAction(IActionManager.SET, constraint, up
					.getConstraint_Body(), body, -1);
			actionManager.doAction(IActionManager.ADD, owner, up
					.getMethodElement_OwnedRules(), constraint, -1);
		}
		return constraint;
	}

	public static final List<Constraint> getWorkProductStates(WorkProduct wp) {
		List<Constraint> states = new ArrayList<Constraint>();

		if (wp != null) {
			for (Constraint constraint : wp.getOwnedRules()) {
				if (constraint.getName().equals(WORKPRODUCT_State)) {
					states.add(constraint);
				}
			}
		}

		return states;
	}

	public static void addWpState(TaskDescriptor td, WorkProductDescriptor wpd,
			Constraint state, EReference ref, IActionManager actionManager) {
		if (td == null || wpd == null || state == null || ref == null) {
			return;
		}

		// Find the wp states holder constraint object for wpd
		Constraint wpStatesHolder = null;
		for (Constraint constraint : td.getOwnedRules()) {
			if (constraint.getName().equals(TASKDESCRIPTOR_WpStates)
					&& constraint.getBody().equals(wpd.getGuid())) {
				wpStatesHolder = constraint;
				break;
			}
		}
		if (wpStatesHolder == null) {
			wpStatesHolder = createConstraint(td, TASKDESCRIPTOR_WpStates, wpd
					.getGuid(), actionManager);
		}

		MethodElementPropUtil propUtil = new MethodElementPropUtil(actionManager);
		propUtil.addReferenceInfo(wpStatesHolder, state,
				MethodElementPropUtil.CONSTRAINT_WPStates, ref.getName());

	}

	public static void removeWpState(TaskDescriptor td,
			WorkProductDescriptor wpd, Constraint state, EReference ref,
			IActionManager actionManager) {
		if (td == null || wpd == null || state == null || ref == null) {
			return;
		}

		// Find the wp states holder constraint object for wpd
		Constraint wpStatesHolder = null;
		for (Constraint constraint : td.getOwnedRules()) {
			if (constraint.getName().equals(TASKDESCRIPTOR_WpStates)
					&& constraint.getBody().equals(wpd.getGuid())) {
				wpStatesHolder = constraint;
				break;
			}
		}
		if (wpStatesHolder == null) {
			return;
		}

		MethodElementPropUtil propUtil = new MethodElementPropUtil(actionManager);
		propUtil.removeReferenceInfo(wpStatesHolder, state,
				MethodElementPropUtil.CONSTRAINT_WPStates, ref.getName());

	}

	public static List<Constraint> getWpStates(TaskDescriptor td,
			WorkProductDescriptor wpd, EReference ref) {

		Constraint wpStatesHolder = null;
		for (Constraint constraint : td.getOwnedRules()) {
			if (constraint.getName().equals(TASKDESCRIPTOR_WpStates)
					&& constraint.getBody().equals(wpd.getGuid())) {
				wpStatesHolder = constraint;
				break;
			}
		}

		if (wpStatesHolder == null) {
			return new ArrayList<Constraint>();
		}

		MethodElementPropUtil propUtil = MethodElementPropUtil
				.getMethodElementPropUtil();

		return (List<Constraint>) propUtil.extractElements(wpStatesHolder,
				MethodElementPropUtil.CONSTRAINT_WPStates, ref.getName());

	}

}
