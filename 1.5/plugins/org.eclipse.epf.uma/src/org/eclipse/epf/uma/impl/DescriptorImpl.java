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
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Descriptor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.DescriptorImpl#getIsSynchronizedWithSource <em>Is Synchronized With Source</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class DescriptorImpl extends BreakdownElementImpl implements
		Descriptor {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getIsSynchronizedWithSource() <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsSynchronizedWithSource()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT = Boolean.TRUE;

	/**
	 * The cached value of the '{@link #getIsSynchronizedWithSource() <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsSynchronizedWithSource()
	 * @generated
	 * @ordered
	 */
	protected Boolean isSynchronizedWithSource = IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT;

	/**
	 * This is true if the Is Synchronized With Source attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isSynchronizedWithSourceESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DescriptorImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.DESCRIPTOR;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getIsSynchronizedWithSource() {
		return isSynchronizedWithSource;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsSynchronizedWithSource(Boolean newIsSynchronizedWithSource) {
		Boolean oldIsSynchronizedWithSource = isSynchronizedWithSource;
		isSynchronizedWithSource = newIsSynchronizedWithSource;
		boolean oldIsSynchronizedWithSourceESet = isSynchronizedWithSourceESet;
		isSynchronizedWithSourceESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE,
					oldIsSynchronizedWithSource, isSynchronizedWithSource,
					!oldIsSynchronizedWithSourceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsSynchronizedWithSource() {
		Boolean oldIsSynchronizedWithSource = isSynchronizedWithSource;
		boolean oldIsSynchronizedWithSourceESet = isSynchronizedWithSourceESet;
		isSynchronizedWithSource = IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT;
		isSynchronizedWithSourceESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE,
					oldIsSynchronizedWithSource,
					IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT,
					oldIsSynchronizedWithSourceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsSynchronizedWithSource() {
		return isSynchronizedWithSourceESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			return getIsSynchronizedWithSource();
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
		case UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			setIsSynchronizedWithSource((Boolean) newValue);
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
		case UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			unsetIsSynchronizedWithSource();
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
		case UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			return isSetIsSynchronizedWithSource();
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
		result.append(" (isSynchronizedWithSource: "); //$NON-NLS-1$
		if (isSynchronizedWithSourceESet)
			result.append(isSynchronizedWithSource);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //DescriptorImpl
