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
import org.eclipse.epf.uma.SimpleSemanticModelElement;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Simple Semantic Model Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.SimpleSemanticModelElementImpl#getTypeInfo <em>Type Info</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SimpleSemanticModelElementImpl extends SemanticModelBridgeImpl
		implements SimpleSemanticModelElement {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getTypeInfo() <em>Type Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTypeInfo()
	 * @generated
	 * @ordered
	 */
	protected static final String TYPE_INFO_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getTypeInfo() <em>Type Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTypeInfo()
	 * @generated
	 * @ordered
	 */
	protected String typeInfo = TYPE_INFO_EDEFAULT;

	/**
	 * This is true if the Type Info attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean typeInfoESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SimpleSemanticModelElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.SIMPLE_SEMANTIC_MODEL_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getTypeInfo() {
		return typeInfo;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTypeInfo(String newTypeInfo) {
		String oldTypeInfo = typeInfo;
		typeInfo = newTypeInfo;
		boolean oldTypeInfoESet = typeInfoESet;
		typeInfoESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.SIMPLE_SEMANTIC_MODEL_ELEMENT__TYPE_INFO,
					oldTypeInfo, typeInfo, !oldTypeInfoESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetTypeInfo() {
		String oldTypeInfo = typeInfo;
		boolean oldTypeInfoESet = typeInfoESet;
		typeInfo = TYPE_INFO_EDEFAULT;
		typeInfoESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.SIMPLE_SEMANTIC_MODEL_ELEMENT__TYPE_INFO,
					oldTypeInfo, TYPE_INFO_EDEFAULT, oldTypeInfoESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetTypeInfo() {
		return typeInfoESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.SIMPLE_SEMANTIC_MODEL_ELEMENT__TYPE_INFO:
			return getTypeInfo();
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
		case UmaPackage.SIMPLE_SEMANTIC_MODEL_ELEMENT__TYPE_INFO:
			setTypeInfo((String) newValue);
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
		case UmaPackage.SIMPLE_SEMANTIC_MODEL_ELEMENT__TYPE_INFO:
			unsetTypeInfo();
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
		case UmaPackage.SIMPLE_SEMANTIC_MODEL_ELEMENT__TYPE_INFO:
			return isSetTypeInfo();
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
		result.append(" (typeInfo: "); //$NON-NLS-1$
		if (typeInfoESet)
			result.append(typeInfo);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //SimpleSemanticModelElementImpl
