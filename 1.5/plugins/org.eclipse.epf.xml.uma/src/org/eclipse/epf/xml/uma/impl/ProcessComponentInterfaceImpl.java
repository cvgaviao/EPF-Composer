/**
 * <copyright>
 * </copyright>
 *
 * $Id: ProcessComponentInterfaceImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
 */
package org.eclipse.epf.xml.uma.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.BasicFeatureMap;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.xml.uma.ProcessComponentInterface;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Process Component Interface</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ProcessComponentInterfaceImpl#getGroup1 <em>Group1</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ProcessComponentInterfaceImpl#getInterfaceSpecification <em>Interface Specification</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ProcessComponentInterfaceImpl#getInterfaceIO <em>Interface IO</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ProcessComponentInterfaceImpl extends BreakdownElementImpl implements ProcessComponentInterface {
	/**
	 * The cached value of the '{@link #getGroup1() <em>Group1</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGroup1()
	 * @generated
	 * @ordered
	 */
	protected FeatureMap group1;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ProcessComponentInterfaceImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.PROCESS_COMPONENT_INTERFACE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FeatureMap getGroup1() {
		if (group1 == null) {
			group1 = new BasicFeatureMap(this, UmaPackage.PROCESS_COMPONENT_INTERFACE__GROUP1);
		}
		return group1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getInterfaceSpecification() {
		return getGroup1().list(UmaPackage.Literals.PROCESS_COMPONENT_INTERFACE__INTERFACE_SPECIFICATION);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getInterfaceIO() {
		return getGroup1().list(UmaPackage.Literals.PROCESS_COMPONENT_INTERFACE__INTERFACE_IO);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__GROUP1:
				return ((InternalEList)getGroup1()).basicRemove(otherEnd, msgs);
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_SPECIFICATION:
				return ((InternalEList)getInterfaceSpecification()).basicRemove(otherEnd, msgs);
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_IO:
				return ((InternalEList)getInterfaceIO()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__GROUP1:
				if (coreType) return getGroup1();
				return ((FeatureMap.Internal)getGroup1()).getWrapper();
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_SPECIFICATION:
				return getInterfaceSpecification();
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_IO:
				return getInterfaceIO();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__GROUP1:
				((FeatureMap.Internal)getGroup1()).set(newValue);
				return;
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_SPECIFICATION:
				getInterfaceSpecification().clear();
				getInterfaceSpecification().addAll((Collection)newValue);
				return;
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_IO:
				getInterfaceIO().clear();
				getInterfaceIO().addAll((Collection)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(int featureID) {
		switch (featureID) {
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__GROUP1:
				getGroup1().clear();
				return;
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_SPECIFICATION:
				getInterfaceSpecification().clear();
				return;
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_IO:
				getInterfaceIO().clear();
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__GROUP1:
				return group1 != null && !group1.isEmpty();
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_SPECIFICATION:
				return !getInterfaceSpecification().isEmpty();
			case UmaPackage.PROCESS_COMPONENT_INTERFACE__INTERFACE_IO:
				return !getInterfaceIO().isEmpty();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (group1: ");
		result.append(group1);
		result.append(')');
		return result.toString();
	}

} //ProcessComponentInterfaceImpl