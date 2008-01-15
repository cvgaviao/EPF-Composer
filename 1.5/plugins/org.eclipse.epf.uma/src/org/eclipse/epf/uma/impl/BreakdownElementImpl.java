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
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.PlanningData;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Breakdown Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getPrefix <em>Prefix</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getIsPlanned <em>Is Planned</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getHasMultipleOccurrences <em>Has Multiple Occurrences</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getIsOptional <em>Is Optional</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getPresentedAfter <em>Presented After</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getPresentedBefore <em>Presented Before</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getPlanningData <em>Planning Data</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getSuperActivities <em>Super Activities</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class BreakdownElementImpl extends ProcessElementImpl implements
		BreakdownElement {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getPrefix() <em>Prefix</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPrefix()
	 * @generated
	 * @ordered
	 */
	protected static final String PREFIX_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getPrefix() <em>Prefix</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPrefix()
	 * @generated
	 * @ordered
	 */
	protected String prefix = PREFIX_EDEFAULT;

	/**
	 * The default value of the '{@link #getIsPlanned() <em>Is Planned</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsPlanned()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean IS_PLANNED_EDEFAULT = Boolean.TRUE;

	/**
	 * The cached value of the '{@link #getIsPlanned() <em>Is Planned</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsPlanned()
	 * @generated
	 * @ordered
	 */
	protected Boolean isPlanned = IS_PLANNED_EDEFAULT;

	/**
	 * The default value of the '{@link #getHasMultipleOccurrences() <em>Has Multiple Occurrences</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHasMultipleOccurrences()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean HAS_MULTIPLE_OCCURRENCES_EDEFAULT = Boolean.FALSE;

	/**
	 * The cached value of the '{@link #getHasMultipleOccurrences() <em>Has Multiple Occurrences</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHasMultipleOccurrences()
	 * @generated
	 * @ordered
	 */
	protected Boolean hasMultipleOccurrences = HAS_MULTIPLE_OCCURRENCES_EDEFAULT;

	/**
	 * The default value of the '{@link #getIsOptional() <em>Is Optional</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsOptional()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean IS_OPTIONAL_EDEFAULT = Boolean.FALSE;

	/**
	 * The cached value of the '{@link #getIsOptional() <em>Is Optional</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsOptional()
	 * @generated
	 * @ordered
	 */
	protected Boolean isOptional = IS_OPTIONAL_EDEFAULT;

	/**
	 * The cached value of the '{@link #getPresentedAfter() <em>Presented After</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentedAfter()
	 * @generated
	 * @ordered
	 */
	protected BreakdownElement presentedAfter = null;

	/**
	 * The cached value of the '{@link #getPresentedBefore() <em>Presented Before</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentedBefore()
	 * @generated
	 * @ordered
	 */
	protected BreakdownElement presentedBefore = null;

	/**
	 * The cached value of the '{@link #getPlanningData() <em>Planning Data</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPlanningData()
	 * @generated
	 * @ordered
	 */
	protected PlanningData planningData = null;

	/**
	 * The cached value of the '{@link #getSuperActivities() <em>Super Activities</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuperActivities()
	 * @generated
	 * @ordered
	 */
	protected Activity superActivities = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BreakdownElementImpl() {
		super();

		//UMA-->
		reassignDefaultValues();
		//UMA<--
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.BREAKDOWN_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPrefix() {
		return prefix;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPrefix(String newPrefix) {
		String oldPrefix = prefix;
		prefix = newPrefix;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__PREFIX, oldPrefix, prefix));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getIsPlanned() {
		return isPlanned;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsPlanned(Boolean newIsPlanned) {
		Boolean oldIsPlanned = isPlanned;
		isPlanned = newIsPlanned;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED, oldIsPlanned,
					isPlanned));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getHasMultipleOccurrences() {
		return hasMultipleOccurrences;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHasMultipleOccurrences(Boolean newHasMultipleOccurrences) {
		Boolean oldHasMultipleOccurrences = hasMultipleOccurrences;
		hasMultipleOccurrences = newHasMultipleOccurrences;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES,
					oldHasMultipleOccurrences, hasMultipleOccurrences));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getIsOptional() {
		return isOptional;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsOptional(Boolean newIsOptional) {
		Boolean oldIsOptional = isOptional;
		isOptional = newIsOptional;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL, oldIsOptional,
					isOptional));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BreakdownElement getPresentedAfter() {
		if (presentedAfter != null && ((EObject) presentedAfter).eIsProxy()) {
			InternalEObject oldPresentedAfter = (InternalEObject) presentedAfter;
			presentedAfter = (BreakdownElement) eResolveProxy(oldPresentedAfter);
			if (presentedAfter != oldPresentedAfter) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER,
							oldPresentedAfter, presentedAfter));
			}
		}
		return presentedAfter;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BreakdownElement basicGetPresentedAfter() {
		return presentedAfter;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPresentedAfter(BreakdownElement newPresentedAfter) {
		BreakdownElement oldPresentedAfter = presentedAfter;
		presentedAfter = newPresentedAfter;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER,
					oldPresentedAfter, presentedAfter));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BreakdownElement getPresentedBefore() {
		if (presentedBefore != null && ((EObject) presentedBefore).eIsProxy()) {
			InternalEObject oldPresentedBefore = (InternalEObject) presentedBefore;
			presentedBefore = (BreakdownElement) eResolveProxy(oldPresentedBefore);
			if (presentedBefore != oldPresentedBefore) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE,
							oldPresentedBefore, presentedBefore));
			}
		}
		return presentedBefore;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BreakdownElement basicGetPresentedBefore() {
		return presentedBefore;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPresentedBefore(BreakdownElement newPresentedBefore) {
		BreakdownElement oldPresentedBefore = presentedBefore;
		presentedBefore = newPresentedBefore;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE,
					oldPresentedBefore, presentedBefore));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public PlanningData getPlanningData() {
		if (planningData != null && ((EObject) planningData).eIsProxy()) {
			InternalEObject oldPlanningData = (InternalEObject) planningData;
			planningData = (PlanningData) eResolveProxy(oldPlanningData);
			if (planningData != oldPlanningData) {
				InternalEObject newPlanningData = (InternalEObject) planningData;
				NotificationChain msgs = oldPlanningData.eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE
								- UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
						null, null);
				if (newPlanningData.eInternalContainer() == null) {
					msgs = newPlanningData
							.eInverseAdd(
									this,
									EOPPOSITE_FEATURE_BASE
											- UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
									null, msgs);
				}
				if (msgs != null)
					msgs.dispatch();
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
							oldPlanningData, planningData));
			}
		}
		return planningData;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public PlanningData basicGetPlanningData() {
		return planningData;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetPlanningData(PlanningData newPlanningData,
			NotificationChain msgs) {
		PlanningData oldPlanningData = planningData;
		planningData = newPlanningData;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
					oldPlanningData, newPlanningData);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPlanningData(PlanningData newPlanningData) {
		if (newPlanningData != planningData) {
			NotificationChain msgs = null;
			if (planningData != null)
				msgs = ((InternalEObject) planningData).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE
								- UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
						null, msgs);
			if (newPlanningData != null)
				msgs = ((InternalEObject) newPlanningData).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE
								- UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
						null, msgs);
			msgs = basicSetPlanningData(newPlanningData, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA,
					newPlanningData, newPlanningData));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Activity getSuperActivities() {
		if (superActivities != null && ((EObject) superActivities).eIsProxy()) {
			InternalEObject oldSuperActivities = (InternalEObject) superActivities;
			superActivities = (Activity) eResolveProxy(oldSuperActivities);
			if (superActivities != oldSuperActivities) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES,
							oldSuperActivities, superActivities));
			}
		}
		return superActivities;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Activity basicGetSuperActivities() {
		return superActivities;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetSuperActivities(
			Activity newSuperActivities, NotificationChain msgs) {
		Activity oldSuperActivities = superActivities;
		superActivities = newSuperActivities;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES,
					oldSuperActivities, newSuperActivities);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSuperActivities(Activity newSuperActivities) {
		if (newSuperActivities != superActivities) {
			NotificationChain msgs = null;
			if (superActivities != null)
				msgs = ((InternalEObject) superActivities).eInverseRemove(this,
						UmaPackage.ACTIVITY__BREAKDOWN_ELEMENTS,
						Activity.class, msgs);
			if (newSuperActivities != null)
				msgs = ((InternalEObject) newSuperActivities).eInverseAdd(this,
						UmaPackage.ACTIVITY__BREAKDOWN_ELEMENTS,
						Activity.class, msgs);
			msgs = basicSetSuperActivities(newSuperActivities, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES,
					newSuperActivities, newSuperActivities));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseAdd(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			if (superActivities != null)
				msgs = ((InternalEObject) superActivities).eInverseRemove(this,
						UmaPackage.ACTIVITY__BREAKDOWN_ELEMENTS,
						Activity.class, msgs);
			return basicSetSuperActivities((Activity) otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA:
			return basicSetPlanningData(null, msgs);
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			return basicSetSuperActivities(null, msgs);
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
		case UmaPackage.BREAKDOWN_ELEMENT__PREFIX:
			return getPrefix();
		case UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED:
			return getIsPlanned();
		case UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES:
			return getHasMultipleOccurrences();
		case UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL:
			return getIsOptional();
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER:
			if (resolve)
				return getPresentedAfter();
			return basicGetPresentedAfter();
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE:
			if (resolve)
				return getPresentedBefore();
			return basicGetPresentedBefore();
		case UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA:
			if (resolve)
				return getPlanningData();
			return basicGetPlanningData();
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			if (resolve)
				return getSuperActivities();
			return basicGetSuperActivities();
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
		case UmaPackage.BREAKDOWN_ELEMENT__PREFIX:
			setPrefix((String) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED:
			setIsPlanned((Boolean) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES:
			setHasMultipleOccurrences((Boolean) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL:
			setIsOptional((Boolean) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER:
			setPresentedAfter((BreakdownElement) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE:
			setPresentedBefore((BreakdownElement) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA:
			setPlanningData((PlanningData) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			setSuperActivities((Activity) newValue);
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
		case UmaPackage.BREAKDOWN_ELEMENT__PREFIX:
			setPrefix(PREFIX_EDEFAULT);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED:
			setIsPlanned(IS_PLANNED_EDEFAULT);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES:
			setHasMultipleOccurrences(HAS_MULTIPLE_OCCURRENCES_EDEFAULT);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL:
			setIsOptional(IS_OPTIONAL_EDEFAULT);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER:
			setPresentedAfter((BreakdownElement) null);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE:
			setPresentedBefore((BreakdownElement) null);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA:
			setPlanningData((PlanningData) null);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			setSuperActivities((Activity) null);
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
		//UMA-->
		EStructuralFeature feature = getFeatureWithOverridenDefaultValue(featureID);
		if (feature != null) {
			return isFeatureWithOverridenDefaultValueSet(feature);
		}
		//UMA<--		
		switch (featureID) {
		case UmaPackage.BREAKDOWN_ELEMENT__PREFIX:
			return PREFIX_EDEFAULT == null ? prefix != null : !PREFIX_EDEFAULT
					.equals(prefix);
		case UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED:
			return IS_PLANNED_EDEFAULT == null ? isPlanned != null
					: !IS_PLANNED_EDEFAULT.equals(isPlanned);
		case UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES:
			return HAS_MULTIPLE_OCCURRENCES_EDEFAULT == null ? hasMultipleOccurrences != null
					: !HAS_MULTIPLE_OCCURRENCES_EDEFAULT
							.equals(hasMultipleOccurrences);
		case UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL:
			return IS_OPTIONAL_EDEFAULT == null ? isOptional != null
					: !IS_OPTIONAL_EDEFAULT.equals(isOptional);
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER:
			return presentedAfter != null;
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE:
			return presentedBefore != null;
		case UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA:
			return planningData != null;
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			return superActivities != null;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (prefix: "); //$NON-NLS-1$
		result.append(prefix);
		result.append(", isPlanned: "); //$NON-NLS-1$
		result.append(isPlanned);
		result.append(", hasMultipleOccurrences: "); //$NON-NLS-1$
		result.append(hasMultipleOccurrences);
		result.append(", isOptional: "); //$NON-NLS-1$
		result.append(isOptional);
		result.append(')');
		return result.toString();
	}

} //BreakdownElementImpl
