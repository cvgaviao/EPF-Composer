/**
 * <copyright>
 * </copyright>
 *
 * $Id: ExtendedAttributesImpl.java,v 1.1 2008/01/15 08:51:53 jtham Exp $
 */
package org.eclipse.epf.msproject.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.msproject.ExtendedAttribute;
import org.eclipse.epf.msproject.ExtendedAttributes;
import org.eclipse.epf.msproject.MsprojectPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Extended Attributes</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.impl.ExtendedAttributesImpl#getExtendedAttribute <em>Extended Attribute</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExtendedAttributesImpl extends EObjectImpl implements ExtendedAttributes {
	/**
	 * The cached value of the '{@link #getExtendedAttribute() <em>Extended Attribute</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExtendedAttribute()
	 * @generated
	 * @ordered
	 */
	protected EList extendedAttribute = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtendedAttributesImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return MsprojectPackage.eINSTANCE.getExtendedAttributes();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getExtendedAttribute() {
		if (extendedAttribute == null) {
			extendedAttribute = new EObjectContainmentEList(ExtendedAttribute.class, this, MsprojectPackage.EXTENDED_ATTRIBUTES__EXTENDED_ATTRIBUTE);
		}
		return extendedAttribute;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, Class baseClass, NotificationChain msgs) {
		if (featureID >= 0) {
			switch (eDerivedStructuralFeatureID(featureID, baseClass)) {
				case MsprojectPackage.EXTENDED_ATTRIBUTES__EXTENDED_ATTRIBUTE:
					return ((InternalEList)getExtendedAttribute()).basicRemove(otherEnd, msgs);
				default:
					return eDynamicInverseRemove(otherEnd, featureID, baseClass, msgs);
			}
		}
		return eBasicSetContainer(null, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(EStructuralFeature eFeature, boolean resolve) {
		switch (eDerivedStructuralFeatureID(eFeature)) {
			case MsprojectPackage.EXTENDED_ATTRIBUTES__EXTENDED_ATTRIBUTE:
				return getExtendedAttribute();
		}
		return eDynamicGet(eFeature, resolve);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(EStructuralFeature eFeature, Object newValue) {
		switch (eDerivedStructuralFeatureID(eFeature)) {
			case MsprojectPackage.EXTENDED_ATTRIBUTES__EXTENDED_ATTRIBUTE:
				getExtendedAttribute().clear();
				getExtendedAttribute().addAll((Collection)newValue);
				return;
		}
		eDynamicSet(eFeature, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(EStructuralFeature eFeature) {
		switch (eDerivedStructuralFeatureID(eFeature)) {
			case MsprojectPackage.EXTENDED_ATTRIBUTES__EXTENDED_ATTRIBUTE:
				getExtendedAttribute().clear();
				return;
		}
		eDynamicUnset(eFeature);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(EStructuralFeature eFeature) {
		switch (eDerivedStructuralFeatureID(eFeature)) {
			case MsprojectPackage.EXTENDED_ATTRIBUTES__EXTENDED_ATTRIBUTE:
				return extendedAttribute != null && !extendedAttribute.isEmpty();
		}
		return eDynamicIsSet(eFeature);
	}

} //ExtendedAttributesImpl
