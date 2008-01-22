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
import org.eclipse.epf.uma.ActivityDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Activity Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.ActivityDescriptionImpl#getPurpose <em>Purpose</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ActivityDescriptionImpl#getAlternatives <em>Alternatives</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ActivityDescriptionImpl#getHowtoStaff <em>Howto Staff</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ActivityDescriptionImpl extends BreakdownElementDescriptionImpl
		implements ActivityDescription {
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
	 * The default value of the '{@link #getAlternatives() <em>Alternatives</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAlternatives()
	 * @generated
	 * @ordered
	 */
	protected static final String ALTERNATIVES_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getAlternatives() <em>Alternatives</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAlternatives()
	 * @generated
	 * @ordered
	 */
	protected String alternatives = ALTERNATIVES_EDEFAULT;

	/**
	 * This is true if the Alternatives attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean alternativesESet;

	/**
	 * The default value of the '{@link #getHowtoStaff() <em>Howto Staff</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHowtoStaff()
	 * @generated
	 * @ordered
	 */
	protected static final String HOWTO_STAFF_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getHowtoStaff() <em>Howto Staff</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHowtoStaff()
	 * @generated
	 * @ordered
	 */
	protected String howtoStaff = HOWTO_STAFF_EDEFAULT;

	/**
	 * This is true if the Howto Staff attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean howtoStaffESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ActivityDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.ACTIVITY_DESCRIPTION;
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
					UmaPackage.ACTIVITY_DESCRIPTION__PURPOSE, oldPurpose,
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
					UmaPackage.ACTIVITY_DESCRIPTION__PURPOSE, oldPurpose,
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
	public String getAlternatives() {
		return alternatives;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAlternatives(String newAlternatives) {
		String oldAlternatives = alternatives;
		alternatives = newAlternatives;
		boolean oldAlternativesESet = alternativesESet;
		alternativesESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ACTIVITY_DESCRIPTION__ALTERNATIVES,
					oldAlternatives, alternatives, !oldAlternativesESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetAlternatives() {
		String oldAlternatives = alternatives;
		boolean oldAlternativesESet = alternativesESet;
		alternatives = ALTERNATIVES_EDEFAULT;
		alternativesESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ACTIVITY_DESCRIPTION__ALTERNATIVES,
					oldAlternatives, ALTERNATIVES_EDEFAULT, oldAlternativesESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetAlternatives() {
		return alternativesESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getHowtoStaff() {
		return howtoStaff;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHowtoStaff(String newHowtoStaff) {
		String oldHowtoStaff = howtoStaff;
		howtoStaff = newHowtoStaff;
		boolean oldHowtoStaffESet = howtoStaffESet;
		howtoStaffESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ACTIVITY_DESCRIPTION__HOWTO_STAFF,
					oldHowtoStaff, howtoStaff, !oldHowtoStaffESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetHowtoStaff() {
		String oldHowtoStaff = howtoStaff;
		boolean oldHowtoStaffESet = howtoStaffESet;
		howtoStaff = HOWTO_STAFF_EDEFAULT;
		howtoStaffESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ACTIVITY_DESCRIPTION__HOWTO_STAFF,
					oldHowtoStaff, HOWTO_STAFF_EDEFAULT, oldHowtoStaffESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetHowtoStaff() {
		return howtoStaffESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.ACTIVITY_DESCRIPTION__PURPOSE:
			return getPurpose();
		case UmaPackage.ACTIVITY_DESCRIPTION__ALTERNATIVES:
			return getAlternatives();
		case UmaPackage.ACTIVITY_DESCRIPTION__HOWTO_STAFF:
			return getHowtoStaff();
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
		case UmaPackage.ACTIVITY_DESCRIPTION__PURPOSE:
			setPurpose((String) newValue);
			return;
		case UmaPackage.ACTIVITY_DESCRIPTION__ALTERNATIVES:
			setAlternatives((String) newValue);
			return;
		case UmaPackage.ACTIVITY_DESCRIPTION__HOWTO_STAFF:
			setHowtoStaff((String) newValue);
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
		case UmaPackage.ACTIVITY_DESCRIPTION__PURPOSE:
			unsetPurpose();
			return;
		case UmaPackage.ACTIVITY_DESCRIPTION__ALTERNATIVES:
			unsetAlternatives();
			return;
		case UmaPackage.ACTIVITY_DESCRIPTION__HOWTO_STAFF:
			unsetHowtoStaff();
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
		case UmaPackage.ACTIVITY_DESCRIPTION__PURPOSE:
			return isSetPurpose();
		case UmaPackage.ACTIVITY_DESCRIPTION__ALTERNATIVES:
			return isSetAlternatives();
		case UmaPackage.ACTIVITY_DESCRIPTION__HOWTO_STAFF:
			return isSetHowtoStaff();
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
		result.append(", alternatives: "); //$NON-NLS-1$
		if (alternativesESet)
			result.append(alternatives);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", howtoStaff: "); //$NON-NLS-1$
		if (howtoStaffESet)
			result.append(howtoStaff);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //ActivityDescriptionImpl
