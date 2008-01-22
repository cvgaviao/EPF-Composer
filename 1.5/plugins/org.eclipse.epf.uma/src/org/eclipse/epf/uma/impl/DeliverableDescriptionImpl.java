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
import org.eclipse.epf.uma.DeliverableDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Deliverable Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliverableDescriptionImpl#getExternalDescription <em>External Description</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliverableDescriptionImpl#getPackagingGuidance <em>Packaging Guidance</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DeliverableDescriptionImpl extends WorkProductDescriptionImpl
		implements DeliverableDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getExternalDescription() <em>External Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalDescription()
	 * @generated
	 * @ordered
	 */
	protected static final String EXTERNAL_DESCRIPTION_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getExternalDescription() <em>External Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalDescription()
	 * @generated
	 * @ordered
	 */
	protected String externalDescription = EXTERNAL_DESCRIPTION_EDEFAULT;

	/**
	 * This is true if the External Description attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean externalDescriptionESet;

	/**
	 * The default value of the '{@link #getPackagingGuidance() <em>Packaging Guidance</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPackagingGuidance()
	 * @generated
	 * @ordered
	 */
	protected static final String PACKAGING_GUIDANCE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getPackagingGuidance() <em>Packaging Guidance</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPackagingGuidance()
	 * @generated
	 * @ordered
	 */
	protected String packagingGuidance = PACKAGING_GUIDANCE_EDEFAULT;

	/**
	 * This is true if the Packaging Guidance attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean packagingGuidanceESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DeliverableDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.DELIVERABLE_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getExternalDescription() {
		return externalDescription;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setExternalDescription(String newExternalDescription) {
		String oldExternalDescription = externalDescription;
		externalDescription = newExternalDescription;
		boolean oldExternalDescriptionESet = externalDescriptionESet;
		externalDescriptionESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION,
					oldExternalDescription, externalDescription,
					!oldExternalDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetExternalDescription() {
		String oldExternalDescription = externalDescription;
		boolean oldExternalDescriptionESet = externalDescriptionESet;
		externalDescription = EXTERNAL_DESCRIPTION_EDEFAULT;
		externalDescriptionESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION,
					oldExternalDescription, EXTERNAL_DESCRIPTION_EDEFAULT,
					oldExternalDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetExternalDescription() {
		return externalDescriptionESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPackagingGuidance() {
		return packagingGuidance;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPackagingGuidance(String newPackagingGuidance) {
		String oldPackagingGuidance = packagingGuidance;
		packagingGuidance = newPackagingGuidance;
		boolean oldPackagingGuidanceESet = packagingGuidanceESet;
		packagingGuidanceESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE,
					oldPackagingGuidance, packagingGuidance,
					!oldPackagingGuidanceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetPackagingGuidance() {
		String oldPackagingGuidance = packagingGuidance;
		boolean oldPackagingGuidanceESet = packagingGuidanceESet;
		packagingGuidance = PACKAGING_GUIDANCE_EDEFAULT;
		packagingGuidanceESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE,
					oldPackagingGuidance, PACKAGING_GUIDANCE_EDEFAULT,
					oldPackagingGuidanceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetPackagingGuidance() {
		return packagingGuidanceESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
			return getExternalDescription();
		case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
			return getPackagingGuidance();
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
		case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
			setExternalDescription((String) newValue);
			return;
		case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
			setPackagingGuidance((String) newValue);
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
		case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
			unsetExternalDescription();
			return;
		case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
			unsetPackagingGuidance();
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
		case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
			return isSetExternalDescription();
		case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
			return isSetPackagingGuidance();
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
		result.append(" (externalDescription: "); //$NON-NLS-1$
		if (externalDescriptionESet)
			result.append(externalDescription);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", packagingGuidance: "); //$NON-NLS-1$
		if (packagingGuidanceESet)
			result.append(packagingGuidance);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //DeliverableDescriptionImpl
