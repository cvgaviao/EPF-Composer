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

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.epf.uma.Kind;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Kind</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.KindImpl#getApplicableMetaClass <em>Applicable Meta Class</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.KindImpl#getIsPrimaryKind <em>Is Primary Kind</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class KindImpl extends ContentElementImpl implements Kind {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getApplicableMetaClass() <em>Applicable Meta Class</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getApplicableMetaClass()
	 * @generated
	 * @ordered
	 */
	protected static final String APPLICABLE_META_CLASS_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getApplicableMetaClass() <em>Applicable Meta Class</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getApplicableMetaClass()
	 * @generated
	 * @ordered
	 */
	protected String applicableMetaClass = APPLICABLE_META_CLASS_EDEFAULT;

	/**
	 * This is true if the Applicable Meta Class attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean applicableMetaClassESet;

	/**
	 * The default value of the '{@link #getIsPrimaryKind() <em>Is Primary Kind</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsPrimaryKind()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean IS_PRIMARY_KIND_EDEFAULT = Boolean.FALSE;

	/**
	 * The cached value of the '{@link #getIsPrimaryKind() <em>Is Primary Kind</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsPrimaryKind()
	 * @generated
	 * @ordered
	 */
	protected Boolean isPrimaryKind = IS_PRIMARY_KIND_EDEFAULT;

	/**
	 * This is true if the Is Primary Kind attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isPrimaryKindESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected KindImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.KIND;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getApplicableMetaClass() {
		return applicableMetaClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setApplicableMetaClass(String newApplicableMetaClass) {
		String oldApplicableMetaClass = applicableMetaClass;
		applicableMetaClass = newApplicableMetaClass;
		boolean oldApplicableMetaClassESet = applicableMetaClassESet;
		applicableMetaClassESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.KIND__APPLICABLE_META_CLASS,
					oldApplicableMetaClass, applicableMetaClass,
					!oldApplicableMetaClassESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetApplicableMetaClass() {
		String oldApplicableMetaClass = applicableMetaClass;
		boolean oldApplicableMetaClassESet = applicableMetaClassESet;
		applicableMetaClass = APPLICABLE_META_CLASS_EDEFAULT;
		applicableMetaClassESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.KIND__APPLICABLE_META_CLASS,
					oldApplicableMetaClass, APPLICABLE_META_CLASS_EDEFAULT,
					oldApplicableMetaClassESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetApplicableMetaClass() {
		return applicableMetaClassESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getIsPrimaryKind() {
		return isPrimaryKind;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsPrimaryKind(Boolean newIsPrimaryKind) {
		Boolean oldIsPrimaryKind = isPrimaryKind;
		isPrimaryKind = newIsPrimaryKind;
		boolean oldIsPrimaryKindESet = isPrimaryKindESet;
		isPrimaryKindESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.KIND__IS_PRIMARY_KIND, oldIsPrimaryKind,
					isPrimaryKind, !oldIsPrimaryKindESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsPrimaryKind() {
		Boolean oldIsPrimaryKind = isPrimaryKind;
		boolean oldIsPrimaryKindESet = isPrimaryKindESet;
		isPrimaryKind = IS_PRIMARY_KIND_EDEFAULT;
		isPrimaryKindESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.KIND__IS_PRIMARY_KIND, oldIsPrimaryKind,
					IS_PRIMARY_KIND_EDEFAULT, oldIsPrimaryKindESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsPrimaryKind() {
		return isPrimaryKindESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.KIND__APPLICABLE_META_CLASS:
			return getApplicableMetaClass();
		case UmaPackage.KIND__IS_PRIMARY_KIND:
			return getIsPrimaryKind();
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
		case UmaPackage.KIND__APPLICABLE_META_CLASS:
			setApplicableMetaClass((String) newValue);
			return;
		case UmaPackage.KIND__IS_PRIMARY_KIND:
			setIsPrimaryKind((Boolean) newValue);
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
		case UmaPackage.KIND__APPLICABLE_META_CLASS:
			unsetApplicableMetaClass();
			return;
		case UmaPackage.KIND__IS_PRIMARY_KIND:
			unsetIsPrimaryKind();
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
		case UmaPackage.KIND__APPLICABLE_META_CLASS:
			return isSetApplicableMetaClass();
		case UmaPackage.KIND__IS_PRIMARY_KIND:
			return isSetIsPrimaryKind();
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
		result.append(" (applicableMetaClass: "); //$NON-NLS-1$
		if (applicableMetaClassESet)
			result.append(applicableMetaClass);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", isPrimaryKind: "); //$NON-NLS-1$
		if (isPrimaryKindESet)
			result.append(isPrimaryKind);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //KindImpl
