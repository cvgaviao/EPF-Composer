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
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;


/**
 * Helper class that manages the use of Contraint in library to store
 * implementation-specific data
 * 
 * @author Phong Nguyen Le - Dec 1, 2005
 * @author Weiping Lu, Aripl 20, 2010
 * @since 1.0
 */
public final class ConstraintManager {
	/** Constants for contrain name */
	public static final String ACITIVY_DIAGRAM = "diagram"; //$NON-NLS-1$

	public static final String PROCESS_SUPPRESSION = ""; //$NON-NLS-1$
	
	public static final String WORKPRODUCT_STATE = "state"; //$NON-NLS-1$
	
	public static final String TASKDESCRIPTOR_WP_STATES = "wp_states"; //$NON-NLS-1$

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
			String stateName, boolean create) {
		if (wp == null || stateName == null || stateName.trim().length() == 0) {
			return null;
		}

		for (Iterator iter = wp.getOwnedRules().iterator(); iter.hasNext();) {
			Constraint constraint = (Constraint) iter.next();
			if (constraint.getName().equals(WORKPRODUCT_STATE)
					&& constraint.getBody().equals(stateName)) {
				return constraint;
			}
		}

		Constraint constraint = null;
		if (create) {
			constraint = UmaFactory.eINSTANCE.createConstraint();
			constraint.setName(WORKPRODUCT_STATE);
			constraint.setBody(stateName);
			wp.getOwnedRules().add(constraint);
		}

		return constraint;
	}
	
	public static final List<Constraint> getWorkProductStates(WorkProduct wp) {
		List<Constraint> states = new ArrayList<Constraint>();

		if (wp != null) {
			for (Constraint constraint : wp.getOwnedRules()) {
				if (constraint.getName().equals(WORKPRODUCT_STATE)) {
					states.add(constraint);
				}
			}
		}

		return states;
	}
	
	public static void addWpState(TaskDescriptor td, WorkProductDescriptor wpd,
			Constraint state, EReference ref) {
		if (td == null || wpd == null || state == null || ref == null) {
			return;
		}
		
		//Find the wp states holder constraint object for wpd  
		Constraint wpStatesHolder = null;
		for (Constraint constraint : td.getOwnedRules()) {
			if (constraint.getName().equals(TASKDESCRIPTOR_WP_STATES)
					&& constraint.getBody().equals(wpd.getGuid())) {
				wpStatesHolder = constraint;
				break;
			}
		}
		if (wpStatesHolder ==null) {
			wpStatesHolder = UmaFactory.eINSTANCE.createConstraint();
			wpStatesHolder.setName(ref.getName());
			wpStatesHolder.setBody(wpd.getGuid());
			td.getOwnedRules().add(wpStatesHolder);
		}
		
	}
	
	
}
