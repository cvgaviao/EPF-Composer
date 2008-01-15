/**
 * <copyright>
 * </copyright>
 *
 * $Id: TeamProfileImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
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
import org.eclipse.epf.xml.uma.TeamProfile;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Team Profile</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TeamProfileImpl#getGroup1 <em>Group1</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TeamProfileImpl#getRole <em>Role</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TeamProfileImpl#getSuperTeam <em>Super Team</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TeamProfileImpl#getSubTeam <em>Sub Team</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TeamProfileImpl extends BreakdownElementImpl implements TeamProfile {
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
	protected TeamProfileImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.TEAM_PROFILE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FeatureMap getGroup1() {
		if (group1 == null) {
			group1 = new BasicFeatureMap(this, UmaPackage.TEAM_PROFILE__GROUP1);
		}
		return group1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getRole() {
		return getGroup1().list(UmaPackage.Literals.TEAM_PROFILE__ROLE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getSuperTeam() {
		return getGroup1().list(UmaPackage.Literals.TEAM_PROFILE__SUPER_TEAM);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getSubTeam() {
		return getGroup1().list(UmaPackage.Literals.TEAM_PROFILE__SUB_TEAM);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case UmaPackage.TEAM_PROFILE__GROUP1:
				return ((InternalEList)getGroup1()).basicRemove(otherEnd, msgs);
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
			case UmaPackage.TEAM_PROFILE__GROUP1:
				if (coreType) return getGroup1();
				return ((FeatureMap.Internal)getGroup1()).getWrapper();
			case UmaPackage.TEAM_PROFILE__ROLE:
				return getRole();
			case UmaPackage.TEAM_PROFILE__SUPER_TEAM:
				return getSuperTeam();
			case UmaPackage.TEAM_PROFILE__SUB_TEAM:
				return getSubTeam();
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
			case UmaPackage.TEAM_PROFILE__GROUP1:
				((FeatureMap.Internal)getGroup1()).set(newValue);
				return;
			case UmaPackage.TEAM_PROFILE__ROLE:
				getRole().clear();
				getRole().addAll((Collection)newValue);
				return;
			case UmaPackage.TEAM_PROFILE__SUPER_TEAM:
				getSuperTeam().clear();
				getSuperTeam().addAll((Collection)newValue);
				return;
			case UmaPackage.TEAM_PROFILE__SUB_TEAM:
				getSubTeam().clear();
				getSubTeam().addAll((Collection)newValue);
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
			case UmaPackage.TEAM_PROFILE__GROUP1:
				getGroup1().clear();
				return;
			case UmaPackage.TEAM_PROFILE__ROLE:
				getRole().clear();
				return;
			case UmaPackage.TEAM_PROFILE__SUPER_TEAM:
				getSuperTeam().clear();
				return;
			case UmaPackage.TEAM_PROFILE__SUB_TEAM:
				getSubTeam().clear();
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
			case UmaPackage.TEAM_PROFILE__GROUP1:
				return group1 != null && !group1.isEmpty();
			case UmaPackage.TEAM_PROFILE__ROLE:
				return !getRole().isEmpty();
			case UmaPackage.TEAM_PROFILE__SUPER_TEAM:
				return !getSuperTeam().isEmpty();
			case UmaPackage.TEAM_PROFILE__SUB_TEAM:
				return !getSubTeam().isEmpty();
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

} //TeamProfileImpl