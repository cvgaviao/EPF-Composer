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
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescription;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Work Product Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.WorkProductDescriptionImpl#getPurpose <em>Purpose</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.WorkProductDescriptionImpl#getImpactOfNotHaving <em>Impact Of Not Having</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.WorkProductDescriptionImpl#getReasonsForNotNeeding <em>Reasons For Not Needing</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class WorkProductDescriptionImpl extends ContentDescriptionImpl
		implements WorkProductDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getPurpose() <em>Purpose</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPurpose()
	 * @generated
	 * @ordered
	 */
	protected static final String PURPOSE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getPurpose() <em>Purpose</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPurpose()
	 * @generated
	 * @ordered
	 */
	protected String purpose = PURPOSE_EDEFAULT;

	/**
	 * This is true if the Purpose attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean purposeESet;

	/**
	 * The default value of the '{@link #getImpactOfNotHaving() <em>Impact Of Not Having</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getImpactOfNotHaving()
	 * @generated
	 * @ordered
	 */
	protected static final String IMPACT_OF_NOT_HAVING_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getImpactOfNotHaving() <em>Impact Of Not Having</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getImpactOfNotHaving()
	 * @generated
	 * @ordered
	 */
	protected String impactOfNotHaving = IMPACT_OF_NOT_HAVING_EDEFAULT;

	/**
	 * This is true if the Impact Of Not Having attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean impactOfNotHavingESet;

	/**
	 * The default value of the '{@link #getReasonsForNotNeeding() <em>Reasons For Not Needing</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReasonsForNotNeeding()
	 * @generated
	 * @ordered
	 */
	protected static final String REASONS_FOR_NOT_NEEDING_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getReasonsForNotNeeding() <em>Reasons For Not Needing</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReasonsForNotNeeding()
	 * @generated
	 * @ordered
	 */
	protected String reasonsForNotNeeding = REASONS_FOR_NOT_NEEDING_EDEFAULT;

	/**
	 * This is true if the Reasons For Not Needing attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean reasonsForNotNeedingESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected WorkProductDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.WORK_PRODUCT_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPurpose() {
		return purpose;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPurpose(String newPurpose) {
		String oldPurpose = purpose;
		purpose = newPurpose;
		boolean oldPurposeESet = purposeESet;
		purposeESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.WORK_PRODUCT_DESCRIPTION__PURPOSE, oldPurpose,
					purpose, !oldPurposeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetPurpose() {
		String oldPurpose = purpose;
		boolean oldPurposeESet = purposeESet;
		purpose = PURPOSE_EDEFAULT;
		purposeESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.WORK_PRODUCT_DESCRIPTION__PURPOSE, oldPurpose,
					PURPOSE_EDEFAULT, oldPurposeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetPurpose() {
		return purposeESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getImpactOfNotHaving() {
		return impactOfNotHaving;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setImpactOfNotHaving(String newImpactOfNotHaving) {
		String oldImpactOfNotHaving = impactOfNotHaving;
		impactOfNotHaving = newImpactOfNotHaving;
		boolean oldImpactOfNotHavingESet = impactOfNotHavingESet;
		impactOfNotHavingESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.WORK_PRODUCT_DESCRIPTION__IMPACT_OF_NOT_HAVING,
					oldImpactOfNotHaving, impactOfNotHaving,
					!oldImpactOfNotHavingESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetImpactOfNotHaving() {
		String oldImpactOfNotHaving = impactOfNotHaving;
		boolean oldImpactOfNotHavingESet = impactOfNotHavingESet;
		impactOfNotHaving = IMPACT_OF_NOT_HAVING_EDEFAULT;
		impactOfNotHavingESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.WORK_PRODUCT_DESCRIPTION__IMPACT_OF_NOT_HAVING,
					oldImpactOfNotHaving, IMPACT_OF_NOT_HAVING_EDEFAULT,
					oldImpactOfNotHavingESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetImpactOfNotHaving() {
		return impactOfNotHavingESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getReasonsForNotNeeding() {
		return reasonsForNotNeeding;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setReasonsForNotNeeding(String newReasonsForNotNeeding) {
		String oldReasonsForNotNeeding = reasonsForNotNeeding;
		reasonsForNotNeeding = newReasonsForNotNeeding;
		boolean oldReasonsForNotNeedingESet = reasonsForNotNeedingESet;
		reasonsForNotNeedingESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.SET,
					UmaPackage.WORK_PRODUCT_DESCRIPTION__REASONS_FOR_NOT_NEEDING,
					oldReasonsForNotNeeding, reasonsForNotNeeding,
					!oldReasonsForNotNeedingESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetReasonsForNotNeeding() {
		String oldReasonsForNotNeeding = reasonsForNotNeeding;
		boolean oldReasonsForNotNeedingESet = reasonsForNotNeedingESet;
		reasonsForNotNeeding = REASONS_FOR_NOT_NEEDING_EDEFAULT;
		reasonsForNotNeedingESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.UNSET,
					UmaPackage.WORK_PRODUCT_DESCRIPTION__REASONS_FOR_NOT_NEEDING,
					oldReasonsForNotNeeding, REASONS_FOR_NOT_NEEDING_EDEFAULT,
					oldReasonsForNotNeedingESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetReasonsForNotNeeding() {
		return reasonsForNotNeedingESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__PURPOSE:
			return getPurpose();
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__IMPACT_OF_NOT_HAVING:
			return getImpactOfNotHaving();
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__REASONS_FOR_NOT_NEEDING:
			return getReasonsForNotNeeding();
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
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__PURPOSE:
			setPurpose((String) newValue);
			return;
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__IMPACT_OF_NOT_HAVING:
			setImpactOfNotHaving((String) newValue);
			return;
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__REASONS_FOR_NOT_NEEDING:
			setReasonsForNotNeeding((String) newValue);
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
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__PURPOSE:
			unsetPurpose();
			return;
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__IMPACT_OF_NOT_HAVING:
			unsetImpactOfNotHaving();
			return;
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__REASONS_FOR_NOT_NEEDING:
			unsetReasonsForNotNeeding();
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
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__PURPOSE:
			return isSetPurpose();
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__IMPACT_OF_NOT_HAVING:
			return isSetImpactOfNotHaving();
		case UmaPackage.WORK_PRODUCT_DESCRIPTION__REASONS_FOR_NOT_NEEDING:
			return isSetReasonsForNotNeeding();
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
		result.append(" (purpose: "); //$NON-NLS-1$
		if (purposeESet)
			result.append(purpose);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", impactOfNotHaving: "); //$NON-NLS-1$
		if (impactOfNotHavingESet)
			result.append(impactOfNotHaving);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", reasonsForNotNeeding: "); //$NON-NLS-1$
		if (reasonsForNotNeedingESet)
			result.append(reasonsForNotNeeding);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //WorkProductDescriptionImpl
