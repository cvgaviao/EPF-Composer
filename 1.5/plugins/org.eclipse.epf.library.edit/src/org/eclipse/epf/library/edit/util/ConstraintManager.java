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

import java.util.Iterator;

import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.WorkProduct;


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
		if (wp == null || stateName == null) {
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
			wp.getOwnedRules().add(constraint);
		}

		return constraint;
	}
}
