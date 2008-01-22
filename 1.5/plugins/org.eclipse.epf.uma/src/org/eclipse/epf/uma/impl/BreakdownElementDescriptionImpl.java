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
package org.eclipse.epf.uma.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.BreakdownElementDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Breakdown Element Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementDescriptionImpl#getUsageGuidance <em>Usage Guidance</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BreakdownElementDescriptionImpl extends ContentDescriptionImpl
		implements BreakdownElementDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getUsageGuidance() <em>Usage Guidance</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUsageGuidance()
	 * @generated
	 * @ordered
	 */
	protected static final String USAGE_GUIDANCE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getUsageGuidance() <em>Usage Guidance</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUsageGuidance()
	 * @generated
	 * @ordered
	 */
	protected String usageGuidance = USAGE_GUIDANCE_EDEFAULT;

	/**
	 * This is true if the Usage Guidance attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean usageGuidanceESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BreakdownElementDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.BREAKDOWN_ELEMENT_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getUsageGuidance() {
		return usageGuidance;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setUsageGuidance(String newUsageGuidance) {
		String oldUsageGuidance = usageGuidance;
		usageGuidance = newUsageGuidance;
		boolean oldUsageGuidanceESet = usageGuidanceESet;
		usageGuidanceESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT_DESCRIPTION__USAGE_GUIDANCE,
					oldUsageGuidance, usageGuidance, !oldUsageGuidanceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetUsageGuidance() {
		String oldUsageGuidance = usageGuidance;
		boolean oldUsageGuidanceESet = usageGuidanceESet;
		usageGuidance = USAGE_GUIDANCE_EDEFAULT;
		usageGuidanceESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.BREAKDOWN_ELEMENT_DESCRIPTION__USAGE_GUIDANCE,
					oldUsageGuidance, USAGE_GUIDANCE_EDEFAULT,
					oldUsageGuidanceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetUsageGuidance() {
		return usageGuidanceESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT_DESCRIPTION__USAGE_GUIDANCE:
			return getUsageGuidance();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT_DESCRIPTION__USAGE_GUIDANCE:
			setUsageGuidance((String) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT_DESCRIPTION__USAGE_GUIDANCE:
			unsetUsageGuidance();
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT_DESCRIPTION__USAGE_GUIDANCE:
			return isSetUsageGuidance();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (usageGuidance: "); //$NON-NLS-1$
		if (usageGuidanceESet)
			result.append(usageGuidance);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //BreakdownElementDescriptionImpl
