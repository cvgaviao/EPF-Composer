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
import org.eclipse.epf.uma.PracticeDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Practice Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.PracticeDescriptionImpl#getAdditionalInfo <em>Additional Info</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PracticeDescriptionImpl#getProblem <em>Problem</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PracticeDescriptionImpl#getBackground <em>Background</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PracticeDescriptionImpl#getGoals <em>Goals</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PracticeDescriptionImpl#getApplication <em>Application</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PracticeDescriptionImpl#getLevelsOfAdoption <em>Levels Of Adoption</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PracticeDescriptionImpl extends ContentDescriptionImpl implements
		PracticeDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getAdditionalInfo() <em>Additional Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdditionalInfo()
	 * @generated
	 * @ordered
	 */
	protected static final String ADDITIONAL_INFO_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getAdditionalInfo() <em>Additional Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdditionalInfo()
	 * @generated
	 * @ordered
	 */
	protected String additionalInfo = ADDITIONAL_INFO_EDEFAULT;

	/**
	 * This is true if the Additional Info attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean additionalInfoESet;

	/**
	 * The default value of the '{@link #getProblem() <em>Problem</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProblem()
	 * @generated
	 * @ordered
	 */
	protected static final String PROBLEM_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getProblem() <em>Problem</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProblem()
	 * @generated
	 * @ordered
	 */
	protected String problem = PROBLEM_EDEFAULT;

	/**
	 * This is true if the Problem attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean problemESet;

	/**
	 * The default value of the '{@link #getBackground() <em>Background</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBackground()
	 * @generated
	 * @ordered
	 */
	protected static final String BACKGROUND_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getBackground() <em>Background</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBackground()
	 * @generated
	 * @ordered
	 */
	protected String background = BACKGROUND_EDEFAULT;

	/**
	 * This is true if the Background attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean backgroundESet;

	/**
	 * The default value of the '{@link #getGoals() <em>Goals</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGoals()
	 * @generated
	 * @ordered
	 */
	protected static final String GOALS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getGoals() <em>Goals</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGoals()
	 * @generated
	 * @ordered
	 */
	protected String goals = GOALS_EDEFAULT;

	/**
	 * This is true if the Goals attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean goalsESet;

	/**
	 * The default value of the '{@link #getApplication() <em>Application</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getApplication()
	 * @generated
	 * @ordered
	 */
	protected static final String APPLICATION_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getApplication() <em>Application</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getApplication()
	 * @generated
	 * @ordered
	 */
	protected String application = APPLICATION_EDEFAULT;

	/**
	 * This is true if the Application attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean applicationESet;

	/**
	 * The default value of the '{@link #getLevelsOfAdoption() <em>Levels Of Adoption</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLevelsOfAdoption()
	 * @generated
	 * @ordered
	 */
	protected static final String LEVELS_OF_ADOPTION_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getLevelsOfAdoption() <em>Levels Of Adoption</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLevelsOfAdoption()
	 * @generated
	 * @ordered
	 */
	protected String levelsOfAdoption = LEVELS_OF_ADOPTION_EDEFAULT;

	/**
	 * This is true if the Levels Of Adoption attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean levelsOfAdoptionESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected PracticeDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.PRACTICE_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAdditionalInfo() {
		return additionalInfo;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAdditionalInfo(String newAdditionalInfo) {
		String oldAdditionalInfo = additionalInfo;
		additionalInfo = newAdditionalInfo;
		boolean oldAdditionalInfoESet = additionalInfoESet;
		additionalInfoESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PRACTICE_DESCRIPTION__ADDITIONAL_INFO,
					oldAdditionalInfo, additionalInfo, !oldAdditionalInfoESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetAdditionalInfo() {
		String oldAdditionalInfo = additionalInfo;
		boolean oldAdditionalInfoESet = additionalInfoESet;
		additionalInfo = ADDITIONAL_INFO_EDEFAULT;
		additionalInfoESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PRACTICE_DESCRIPTION__ADDITIONAL_INFO,
					oldAdditionalInfo, ADDITIONAL_INFO_EDEFAULT,
					oldAdditionalInfoESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetAdditionalInfo() {
		return additionalInfoESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getProblem() {
		return problem;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setProblem(String newProblem) {
		String oldProblem = problem;
		problem = newProblem;
		boolean oldProblemESet = problemESet;
		problemESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PRACTICE_DESCRIPTION__PROBLEM, oldProblem,
					problem, !oldProblemESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetProblem() {
		String oldProblem = problem;
		boolean oldProblemESet = problemESet;
		problem = PROBLEM_EDEFAULT;
		problemESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PRACTICE_DESCRIPTION__PROBLEM, oldProblem,
					PROBLEM_EDEFAULT, oldProblemESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetProblem() {
		return problemESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getBackground() {
		return background;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBackground(String newBackground) {
		String oldBackground = background;
		background = newBackground;
		boolean oldBackgroundESet = backgroundESet;
		backgroundESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PRACTICE_DESCRIPTION__BACKGROUND, oldBackground,
					background, !oldBackgroundESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetBackground() {
		String oldBackground = background;
		boolean oldBackgroundESet = backgroundESet;
		background = BACKGROUND_EDEFAULT;
		backgroundESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PRACTICE_DESCRIPTION__BACKGROUND, oldBackground,
					BACKGROUND_EDEFAULT, oldBackgroundESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetBackground() {
		return backgroundESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getGoals() {
		return goals;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGoals(String newGoals) {
		String oldGoals = goals;
		goals = newGoals;
		boolean oldGoalsESet = goalsESet;
		goalsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PRACTICE_DESCRIPTION__GOALS, oldGoals, goals,
					!oldGoalsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetGoals() {
		String oldGoals = goals;
		boolean oldGoalsESet = goalsESet;
		goals = GOALS_EDEFAULT;
		goalsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PRACTICE_DESCRIPTION__GOALS, oldGoals,
					GOALS_EDEFAULT, oldGoalsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetGoals() {
		return goalsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getApplication() {
		return application;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setApplication(String newApplication) {
		String oldApplication = application;
		application = newApplication;
		boolean oldApplicationESet = applicationESet;
		applicationESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PRACTICE_DESCRIPTION__APPLICATION,
					oldApplication, application, !oldApplicationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetApplication() {
		String oldApplication = application;
		boolean oldApplicationESet = applicationESet;
		application = APPLICATION_EDEFAULT;
		applicationESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PRACTICE_DESCRIPTION__APPLICATION,
					oldApplication, APPLICATION_EDEFAULT, oldApplicationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetApplication() {
		return applicationESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLevelsOfAdoption() {
		return levelsOfAdoption;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLevelsOfAdoption(String newLevelsOfAdoption) {
		String oldLevelsOfAdoption = levelsOfAdoption;
		levelsOfAdoption = newLevelsOfAdoption;
		boolean oldLevelsOfAdoptionESet = levelsOfAdoptionESet;
		levelsOfAdoptionESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PRACTICE_DESCRIPTION__LEVELS_OF_ADOPTION,
					oldLevelsOfAdoption, levelsOfAdoption,
					!oldLevelsOfAdoptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetLevelsOfAdoption() {
		String oldLevelsOfAdoption = levelsOfAdoption;
		boolean oldLevelsOfAdoptionESet = levelsOfAdoptionESet;
		levelsOfAdoption = LEVELS_OF_ADOPTION_EDEFAULT;
		levelsOfAdoptionESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PRACTICE_DESCRIPTION__LEVELS_OF_ADOPTION,
					oldLevelsOfAdoption, LEVELS_OF_ADOPTION_EDEFAULT,
					oldLevelsOfAdoptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetLevelsOfAdoption() {
		return levelsOfAdoptionESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.PRACTICE_DESCRIPTION__ADDITIONAL_INFO:
			return getAdditionalInfo();
		case UmaPackage.PRACTICE_DESCRIPTION__PROBLEM:
			return getProblem();
		case UmaPackage.PRACTICE_DESCRIPTION__BACKGROUND:
			return getBackground();
		case UmaPackage.PRACTICE_DESCRIPTION__GOALS:
			return getGoals();
		case UmaPackage.PRACTICE_DESCRIPTION__APPLICATION:
			return getApplication();
		case UmaPackage.PRACTICE_DESCRIPTION__LEVELS_OF_ADOPTION:
			return getLevelsOfAdoption();
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
		case UmaPackage.PRACTICE_DESCRIPTION__ADDITIONAL_INFO:
			setAdditionalInfo((String) newValue);
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__PROBLEM:
			setProblem((String) newValue);
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__BACKGROUND:
			setBackground((String) newValue);
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__GOALS:
			setGoals((String) newValue);
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__APPLICATION:
			setApplication((String) newValue);
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__LEVELS_OF_ADOPTION:
			setLevelsOfAdoption((String) newValue);
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
		case UmaPackage.PRACTICE_DESCRIPTION__ADDITIONAL_INFO:
			unsetAdditionalInfo();
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__PROBLEM:
			unsetProblem();
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__BACKGROUND:
			unsetBackground();
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__GOALS:
			unsetGoals();
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__APPLICATION:
			unsetApplication();
			return;
		case UmaPackage.PRACTICE_DESCRIPTION__LEVELS_OF_ADOPTION:
			unsetLevelsOfAdoption();
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
		case UmaPackage.PRACTICE_DESCRIPTION__ADDITIONAL_INFO:
			return isSetAdditionalInfo();
		case UmaPackage.PRACTICE_DESCRIPTION__PROBLEM:
			return isSetProblem();
		case UmaPackage.PRACTICE_DESCRIPTION__BACKGROUND:
			return isSetBackground();
		case UmaPackage.PRACTICE_DESCRIPTION__GOALS:
			return isSetGoals();
		case UmaPackage.PRACTICE_DESCRIPTION__APPLICATION:
			return isSetApplication();
		case UmaPackage.PRACTICE_DESCRIPTION__LEVELS_OF_ADOPTION:
			return isSetLevelsOfAdoption();
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
		result.append(" (additionalInfo: "); //$NON-NLS-1$
		if (additionalInfoESet)
			result.append(additionalInfo);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", problem: "); //$NON-NLS-1$
		if (problemESet)
			result.append(problem);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", background: "); //$NON-NLS-1$
		if (backgroundESet)
			result.append(background);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", goals: "); //$NON-NLS-1$
		if (goalsESet)
			result.append(goals);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", application: "); //$NON-NLS-1$
		if (applicationESet)
			result.append(application);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", levelsOfAdoption: "); //$NON-NLS-1$
		if (levelsOfAdoptionESet)
			result.append(levelsOfAdoption);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //PracticeDescriptionImpl
