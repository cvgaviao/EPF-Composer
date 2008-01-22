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
import org.eclipse.epf.uma.RoleDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Role Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.RoleDescriptionImpl#getSkills <em>Skills</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.RoleDescriptionImpl#getAssignmentApproaches <em>Assignment Approaches</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.RoleDescriptionImpl#getSynonyms <em>Synonyms</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RoleDescriptionImpl extends ContentDescriptionImpl implements
		RoleDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getSkills() <em>Skills</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSkills()
	 * @generated
	 * @ordered
	 */
	protected static final String SKILLS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getSkills() <em>Skills</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSkills()
	 * @generated
	 * @ordered
	 */
	protected String skills = SKILLS_EDEFAULT;

	/**
	 * This is true if the Skills attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean skillsESet;

	/**
	 * The default value of the '{@link #getAssignmentApproaches() <em>Assignment Approaches</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAssignmentApproaches()
	 * @generated
	 * @ordered
	 */
	protected static final String ASSIGNMENT_APPROACHES_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getAssignmentApproaches() <em>Assignment Approaches</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAssignmentApproaches()
	 * @generated
	 * @ordered
	 */
	protected String assignmentApproaches = ASSIGNMENT_APPROACHES_EDEFAULT;

	/**
	 * This is true if the Assignment Approaches attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean assignmentApproachesESet;

	/**
	 * The default value of the '{@link #getSynonyms() <em>Synonyms</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSynonyms()
	 * @generated
	 * @ordered
	 */
	protected static final String SYNONYMS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getSynonyms() <em>Synonyms</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSynonyms()
	 * @generated
	 * @ordered
	 */
	protected String synonyms = SYNONYMS_EDEFAULT;

	/**
	 * This is true if the Synonyms attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean synonymsESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RoleDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.ROLE_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSkills() {
		return skills;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSkills(String newSkills) {
		String oldSkills = skills;
		skills = newSkills;
		boolean oldSkillsESet = skillsESet;
		skillsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ROLE_DESCRIPTION__SKILLS, oldSkills, skills,
					!oldSkillsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSkills() {
		String oldSkills = skills;
		boolean oldSkillsESet = skillsESet;
		skills = SKILLS_EDEFAULT;
		skillsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ROLE_DESCRIPTION__SKILLS, oldSkills,
					SKILLS_EDEFAULT, oldSkillsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSkills() {
		return skillsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAssignmentApproaches() {
		return assignmentApproaches;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAssignmentApproaches(String newAssignmentApproaches) {
		String oldAssignmentApproaches = assignmentApproaches;
		assignmentApproaches = newAssignmentApproaches;
		boolean oldAssignmentApproachesESet = assignmentApproachesESet;
		assignmentApproachesESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ROLE_DESCRIPTION__ASSIGNMENT_APPROACHES,
					oldAssignmentApproaches, assignmentApproaches,
					!oldAssignmentApproachesESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetAssignmentApproaches() {
		String oldAssignmentApproaches = assignmentApproaches;
		boolean oldAssignmentApproachesESet = assignmentApproachesESet;
		assignmentApproaches = ASSIGNMENT_APPROACHES_EDEFAULT;
		assignmentApproachesESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ROLE_DESCRIPTION__ASSIGNMENT_APPROACHES,
					oldAssignmentApproaches, ASSIGNMENT_APPROACHES_EDEFAULT,
					oldAssignmentApproachesESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetAssignmentApproaches() {
		return assignmentApproachesESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSynonyms() {
		return synonyms;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSynonyms(String newSynonyms) {
		String oldSynonyms = synonyms;
		synonyms = newSynonyms;
		boolean oldSynonymsESet = synonymsESet;
		synonymsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ROLE_DESCRIPTION__SYNONYMS, oldSynonyms,
					synonyms, !oldSynonymsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSynonyms() {
		String oldSynonyms = synonyms;
		boolean oldSynonymsESet = synonymsESet;
		synonyms = SYNONYMS_EDEFAULT;
		synonymsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ROLE_DESCRIPTION__SYNONYMS, oldSynonyms,
					SYNONYMS_EDEFAULT, oldSynonymsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSynonyms() {
		return synonymsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.ROLE_DESCRIPTION__SKILLS:
			return getSkills();
		case UmaPackage.ROLE_DESCRIPTION__ASSIGNMENT_APPROACHES:
			return getAssignmentApproaches();
		case UmaPackage.ROLE_DESCRIPTION__SYNONYMS:
			return getSynonyms();
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
		case UmaPackage.ROLE_DESCRIPTION__SKILLS:
			setSkills((String) newValue);
			return;
		case UmaPackage.ROLE_DESCRIPTION__ASSIGNMENT_APPROACHES:
			setAssignmentApproaches((String) newValue);
			return;
		case UmaPackage.ROLE_DESCRIPTION__SYNONYMS:
			setSynonyms((String) newValue);
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
		case UmaPackage.ROLE_DESCRIPTION__SKILLS:
			unsetSkills();
			return;
		case UmaPackage.ROLE_DESCRIPTION__ASSIGNMENT_APPROACHES:
			unsetAssignmentApproaches();
			return;
		case UmaPackage.ROLE_DESCRIPTION__SYNONYMS:
			unsetSynonyms();
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
		case UmaPackage.ROLE_DESCRIPTION__SKILLS:
			return isSetSkills();
		case UmaPackage.ROLE_DESCRIPTION__ASSIGNMENT_APPROACHES:
			return isSetAssignmentApproaches();
		case UmaPackage.ROLE_DESCRIPTION__SYNONYMS:
			return isSetSynonyms();
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
		result.append(" (skills: "); //$NON-NLS-1$
		if (skillsESet)
			result.append(skills);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", assignmentApproaches: "); //$NON-NLS-1$
		if (assignmentApproachesESet)
			result.append(assignmentApproaches);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", synonyms: "); //$NON-NLS-1$
		if (synonymsESet)
			result.append(synonyms);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //RoleDescriptionImpl
