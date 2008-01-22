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

import java.util.Collection;
import java.util.List;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.Concept;
import org.eclipse.epf.uma.Example;
import org.eclipse.epf.uma.Guideline;
import org.eclipse.epf.uma.PlanningData;
import org.eclipse.epf.uma.ReusableAsset;
import org.eclipse.epf.uma.SupportingMaterial;
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
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getChecklists <em>Checklists</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getConcepts <em>Concepts</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getExamples <em>Examples</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getGuidelines <em>Guidelines</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getReusableAssets <em>Reusable Assets</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.BreakdownElementImpl#getSupportingMaterials <em>Supporting Materials</em>}</li>
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
	 * This is true if the Prefix attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean prefixESet;

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
	 * This is true if the Is Planned attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isPlannedESet;

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
	 * This is true if the Has Multiple Occurrences attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean hasMultipleOccurrencesESet;

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
	 * This is true if the Is Optional attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isOptionalESet;

	/**
	 * The cached value of the '{@link #getPresentedAfter() <em>Presented After</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentedAfter()
	 * @generated
	 * @ordered
	 */
	protected BreakdownElement presentedAfter;

	/**
	 * The cached value of the '{@link #getPresentedBefore() <em>Presented Before</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentedBefore()
	 * @generated
	 * @ordered
	 */
	protected BreakdownElement presentedBefore;

	/**
	 * The cached value of the '{@link #getPlanningData() <em>Planning Data</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPlanningData()
	 * @generated
	 * @ordered
	 */
	protected PlanningData planningData;

	/**
	 * The cached value of the '{@link #getSuperActivities() <em>Super Activities</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuperActivities()
	 * @generated
	 * @ordered
	 */
	protected Activity superActivities;

	/**
	 * The cached value of the '{@link #getChecklists() <em>Checklists</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChecklists()
	 * @generated
	 * @ordered
	 */
	protected EList<Checklist> checklists;

	/**
	 * The cached value of the '{@link #getConcepts() <em>Concepts</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getConcepts()
	 * @generated
	 * @ordered
	 */
	protected EList<Concept> concepts;

	/**
	 * The cached value of the '{@link #getExamples() <em>Examples</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExamples()
	 * @generated
	 * @ordered
	 */
	protected EList<Example> examples;

	/**
	 * The cached value of the '{@link #getGuidelines() <em>Guidelines</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGuidelines()
	 * @generated
	 * @ordered
	 */
	protected EList<Guideline> guidelines;

	/**
	 * The cached value of the '{@link #getReusableAssets() <em>Reusable Assets</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReusableAssets()
	 * @generated
	 * @ordered
	 */
	protected EList<ReusableAsset> reusableAssets;

	/**
	 * The cached value of the '{@link #getSupportingMaterials() <em>Supporting Materials</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSupportingMaterials()
	 * @generated
	 * @ordered
	 */
	protected EList<SupportingMaterial> supportingMaterials;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BreakdownElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
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
		boolean oldPrefixESet = prefixESet;
		prefixESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__PREFIX, oldPrefix, prefix,
					!oldPrefixESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetPrefix() {
		String oldPrefix = prefix;
		boolean oldPrefixESet = prefixESet;
		prefix = PREFIX_EDEFAULT;
		prefixESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.BREAKDOWN_ELEMENT__PREFIX, oldPrefix,
					PREFIX_EDEFAULT, oldPrefixESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetPrefix() {
		return prefixESet;
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
		boolean oldIsPlannedESet = isPlannedESet;
		isPlannedESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED, oldIsPlanned,
					isPlanned, !oldIsPlannedESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsPlanned() {
		Boolean oldIsPlanned = isPlanned;
		boolean oldIsPlannedESet = isPlannedESet;
		isPlanned = IS_PLANNED_EDEFAULT;
		isPlannedESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED, oldIsPlanned,
					IS_PLANNED_EDEFAULT, oldIsPlannedESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsPlanned() {
		return isPlannedESet;
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
		boolean oldHasMultipleOccurrencesESet = hasMultipleOccurrencesESet;
		hasMultipleOccurrencesESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES,
					oldHasMultipleOccurrences, hasMultipleOccurrences,
					!oldHasMultipleOccurrencesESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetHasMultipleOccurrences() {
		Boolean oldHasMultipleOccurrences = hasMultipleOccurrences;
		boolean oldHasMultipleOccurrencesESet = hasMultipleOccurrencesESet;
		hasMultipleOccurrences = HAS_MULTIPLE_OCCURRENCES_EDEFAULT;
		hasMultipleOccurrencesESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES,
					oldHasMultipleOccurrences,
					HAS_MULTIPLE_OCCURRENCES_EDEFAULT,
					oldHasMultipleOccurrencesESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetHasMultipleOccurrences() {
		return hasMultipleOccurrencesESet;
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
		boolean oldIsOptionalESet = isOptionalESet;
		isOptionalESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL, oldIsOptional,
					isOptional, !oldIsOptionalESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsOptional() {
		Boolean oldIsOptional = isOptional;
		boolean oldIsOptionalESet = isOptionalESet;
		isOptional = IS_OPTIONAL_EDEFAULT;
		isOptionalESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL, oldIsOptional,
					IS_OPTIONAL_EDEFAULT, oldIsOptionalESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsOptional() {
		return isOptionalESet;
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
	public List<Checklist> getChecklists() {
		if (checklists == null) {
			checklists = new EObjectResolvingEList<Checklist>(Checklist.class,
					this, UmaPackage.BREAKDOWN_ELEMENT__CHECKLISTS);
		}
		return checklists;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<Concept> getConcepts() {
		if (concepts == null) {
			concepts = new EObjectResolvingEList<Concept>(Concept.class, this,
					UmaPackage.BREAKDOWN_ELEMENT__CONCEPTS);
		}
		return concepts;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<Example> getExamples() {
		if (examples == null) {
			examples = new EObjectResolvingEList<Example>(Example.class, this,
					UmaPackage.BREAKDOWN_ELEMENT__EXAMPLES);
		}
		return examples;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<Guideline> getGuidelines() {
		if (guidelines == null) {
			guidelines = new EObjectResolvingEList<Guideline>(Guideline.class,
					this, UmaPackage.BREAKDOWN_ELEMENT__GUIDELINES);
		}
		return guidelines;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<ReusableAsset> getReusableAssets() {
		if (reusableAssets == null) {
			reusableAssets = new EObjectResolvingEList<ReusableAsset>(
					ReusableAsset.class, this,
					UmaPackage.BREAKDOWN_ELEMENT__REUSABLE_ASSETS);
		}
		return reusableAssets;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<SupportingMaterial> getSupportingMaterials() {
		if (supportingMaterials == null) {
			supportingMaterials = new EObjectResolvingEList<SupportingMaterial>(
					SupportingMaterial.class, this,
					UmaPackage.BREAKDOWN_ELEMENT__SUPPORTING_MATERIALS);
		}
		return supportingMaterials;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
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
	@Override
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
	@Override
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
		case UmaPackage.BREAKDOWN_ELEMENT__CHECKLISTS:
			return getChecklists();
		case UmaPackage.BREAKDOWN_ELEMENT__CONCEPTS:
			return getConcepts();
		case UmaPackage.BREAKDOWN_ELEMENT__EXAMPLES:
			return getExamples();
		case UmaPackage.BREAKDOWN_ELEMENT__GUIDELINES:
			return getGuidelines();
		case UmaPackage.BREAKDOWN_ELEMENT__REUSABLE_ASSETS:
			return getReusableAssets();
		case UmaPackage.BREAKDOWN_ELEMENT__SUPPORTING_MATERIALS:
			return getSupportingMaterials();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
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
		case UmaPackage.BREAKDOWN_ELEMENT__CHECKLISTS:
			getChecklists().clear();
			getChecklists().addAll((Collection<? extends Checklist>) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__CONCEPTS:
			getConcepts().clear();
			getConcepts().addAll((Collection<? extends Concept>) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__EXAMPLES:
			getExamples().clear();
			getExamples().addAll((Collection<? extends Example>) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__GUIDELINES:
			getGuidelines().clear();
			getGuidelines().addAll((Collection<? extends Guideline>) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__REUSABLE_ASSETS:
			getReusableAssets().clear();
			getReusableAssets().addAll(
					(Collection<? extends ReusableAsset>) newValue);
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__SUPPORTING_MATERIALS:
			getSupportingMaterials().clear();
			getSupportingMaterials().addAll(
					(Collection<? extends SupportingMaterial>) newValue);
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
		case UmaPackage.BREAKDOWN_ELEMENT__PREFIX:
			unsetPrefix();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED:
			unsetIsPlanned();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES:
			unsetHasMultipleOccurrences();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL:
			unsetIsOptional();
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
		case UmaPackage.BREAKDOWN_ELEMENT__CHECKLISTS:
			getChecklists().clear();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__CONCEPTS:
			getConcepts().clear();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__EXAMPLES:
			getExamples().clear();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__GUIDELINES:
			getGuidelines().clear();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__REUSABLE_ASSETS:
			getReusableAssets().clear();
			return;
		case UmaPackage.BREAKDOWN_ELEMENT__SUPPORTING_MATERIALS:
			getSupportingMaterials().clear();
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
		case UmaPackage.BREAKDOWN_ELEMENT__PREFIX:
			return isSetPrefix();
		case UmaPackage.BREAKDOWN_ELEMENT__IS_PLANNED:
			return isSetIsPlanned();
		case UmaPackage.BREAKDOWN_ELEMENT__HAS_MULTIPLE_OCCURRENCES:
			return isSetHasMultipleOccurrences();
		case UmaPackage.BREAKDOWN_ELEMENT__IS_OPTIONAL:
			return isSetIsOptional();
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_AFTER:
			return presentedAfter != null;
		case UmaPackage.BREAKDOWN_ELEMENT__PRESENTED_BEFORE:
			return presentedBefore != null;
		case UmaPackage.BREAKDOWN_ELEMENT__PLANNING_DATA:
			return planningData != null;
		case UmaPackage.BREAKDOWN_ELEMENT__SUPER_ACTIVITIES:
			return superActivities != null;
		case UmaPackage.BREAKDOWN_ELEMENT__CHECKLISTS:
			return checklists != null && !checklists.isEmpty();
		case UmaPackage.BREAKDOWN_ELEMENT__CONCEPTS:
			return concepts != null && !concepts.isEmpty();
		case UmaPackage.BREAKDOWN_ELEMENT__EXAMPLES:
			return examples != null && !examples.isEmpty();
		case UmaPackage.BREAKDOWN_ELEMENT__GUIDELINES:
			return guidelines != null && !guidelines.isEmpty();
		case UmaPackage.BREAKDOWN_ELEMENT__REUSABLE_ASSETS:
			return reusableAssets != null && !reusableAssets.isEmpty();
		case UmaPackage.BREAKDOWN_ELEMENT__SUPPORTING_MATERIALS:
			return supportingMaterials != null
					&& !supportingMaterials.isEmpty();
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
		result.append(" (prefix: "); //$NON-NLS-1$
		if (prefixESet)
			result.append(prefix);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", isPlanned: "); //$NON-NLS-1$
		if (isPlannedESet)
			result.append(isPlanned);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", hasMultipleOccurrences: "); //$NON-NLS-1$
		if (hasMultipleOccurrencesESet)
			result.append(hasMultipleOccurrences);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", isOptional: "); //$NON-NLS-1$
		if (isOptionalESet)
			result.append(isOptional);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //BreakdownElementImpl
