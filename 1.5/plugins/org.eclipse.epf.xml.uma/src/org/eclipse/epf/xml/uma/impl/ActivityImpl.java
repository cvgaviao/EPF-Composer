/**
 * <copyright>
 * </copyright>
 *
 * $Id: ActivityImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
 */
package org.eclipse.epf.xml.uma.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.BasicFeatureMap;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.xml.uma.Activity;
import org.eclipse.epf.xml.uma.UmaPackage;
import org.eclipse.epf.xml.uma.VariabilityType;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Activity</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getPrecondition <em>Precondition</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getPostcondition <em>Postcondition</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getGroup2 <em>Group2</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getBreakdownElement <em>Breakdown Element</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getChecklist <em>Checklist</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getConcept <em>Concept</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getExample <em>Example</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getGuideline <em>Guideline</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getRoadmap <em>Roadmap</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getReusableAsset <em>Reusable Asset</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getSupportingMaterial <em>Supporting Material</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getWhitepaper <em>Whitepaper</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#isIsEnactable <em>Is Enactable</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getVariabilityBasedOnElement <em>Variability Based On Element</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.ActivityImpl#getVariabilityType <em>Variability Type</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ActivityImpl extends WorkBreakdownElementImpl implements Activity {
	/**
	 * The default value of the '{@link #getPrecondition() <em>Precondition</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPrecondition()
	 * @generated
	 * @ordered
	 */
	protected static final String PRECONDITION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPrecondition() <em>Precondition</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPrecondition()
	 * @generated
	 * @ordered
	 */
	protected String precondition = PRECONDITION_EDEFAULT;

	/**
	 * The default value of the '{@link #getPostcondition() <em>Postcondition</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPostcondition()
	 * @generated
	 * @ordered
	 */
	protected static final String POSTCONDITION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPostcondition() <em>Postcondition</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPostcondition()
	 * @generated
	 * @ordered
	 */
	protected String postcondition = POSTCONDITION_EDEFAULT;

	/**
	 * The cached value of the '{@link #getGroup2() <em>Group2</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGroup2()
	 * @generated
	 * @ordered
	 */
	protected FeatureMap group2;

	/**
	 * The default value of the '{@link #isIsEnactable() <em>Is Enactable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsEnactable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean IS_ENACTABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isIsEnactable() <em>Is Enactable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsEnactable()
	 * @generated
	 * @ordered
	 */
	protected boolean isEnactable = IS_ENACTABLE_EDEFAULT;

	/**
	 * This is true if the Is Enactable attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isEnactableESet;

	/**
	 * The default value of the '{@link #getVariabilityBasedOnElement() <em>Variability Based On Element</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVariabilityBasedOnElement()
	 * @generated
	 * @ordered
	 */
	protected static final String VARIABILITY_BASED_ON_ELEMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getVariabilityBasedOnElement() <em>Variability Based On Element</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVariabilityBasedOnElement()
	 * @generated
	 * @ordered
	 */
	protected String variabilityBasedOnElement = VARIABILITY_BASED_ON_ELEMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getVariabilityType() <em>Variability Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVariabilityType()
	 * @generated
	 * @ordered
	 */
	protected static final VariabilityType VARIABILITY_TYPE_EDEFAULT = VariabilityType.NA_LITERAL;

	/**
	 * The cached value of the '{@link #getVariabilityType() <em>Variability Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVariabilityType()
	 * @generated
	 * @ordered
	 */
	protected VariabilityType variabilityType = VARIABILITY_TYPE_EDEFAULT;

	/**
	 * This is true if the Variability Type attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean variabilityTypeESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ActivityImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.ACTIVITY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPrecondition() {
		return precondition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPrecondition(String newPrecondition) {
		String oldPrecondition = precondition;
		precondition = newPrecondition;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.ACTIVITY__PRECONDITION, oldPrecondition, precondition));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPostcondition() {
		return postcondition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPostcondition(String newPostcondition) {
		String oldPostcondition = postcondition;
		postcondition = newPostcondition;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.ACTIVITY__POSTCONDITION, oldPostcondition, postcondition));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FeatureMap getGroup2() {
		if (group2 == null) {
			group2 = new BasicFeatureMap(this, UmaPackage.ACTIVITY__GROUP2);
		}
		return group2;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getBreakdownElement() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__BREAKDOWN_ELEMENT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getChecklist() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__CHECKLIST);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getConcept() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__CONCEPT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getExample() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__EXAMPLE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getGuideline() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__GUIDELINE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getRoadmap() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__ROADMAP);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getReusableAsset() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__REUSABLE_ASSET);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getSupportingMaterial() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__SUPPORTING_MATERIAL);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getWhitepaper() {
		return getGroup2().list(UmaPackage.Literals.ACTIVITY__WHITEPAPER);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isIsEnactable() {
		return isEnactable;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsEnactable(boolean newIsEnactable) {
		boolean oldIsEnactable = isEnactable;
		isEnactable = newIsEnactable;
		boolean oldIsEnactableESet = isEnactableESet;
		isEnactableESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.ACTIVITY__IS_ENACTABLE, oldIsEnactable, isEnactable, !oldIsEnactableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsEnactable() {
		boolean oldIsEnactable = isEnactable;
		boolean oldIsEnactableESet = isEnactableESet;
		isEnactable = IS_ENACTABLE_EDEFAULT;
		isEnactableESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, UmaPackage.ACTIVITY__IS_ENACTABLE, oldIsEnactable, IS_ENACTABLE_EDEFAULT, oldIsEnactableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsEnactable() {
		return isEnactableESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getVariabilityBasedOnElement() {
		return variabilityBasedOnElement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVariabilityBasedOnElement(String newVariabilityBasedOnElement) {
		String oldVariabilityBasedOnElement = variabilityBasedOnElement;
		variabilityBasedOnElement = newVariabilityBasedOnElement;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.ACTIVITY__VARIABILITY_BASED_ON_ELEMENT, oldVariabilityBasedOnElement, variabilityBasedOnElement));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public VariabilityType getVariabilityType() {
		return variabilityType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVariabilityType(VariabilityType newVariabilityType) {
		VariabilityType oldVariabilityType = variabilityType;
		variabilityType = newVariabilityType == null ? VARIABILITY_TYPE_EDEFAULT : newVariabilityType;
		boolean oldVariabilityTypeESet = variabilityTypeESet;
		variabilityTypeESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.ACTIVITY__VARIABILITY_TYPE, oldVariabilityType, variabilityType, !oldVariabilityTypeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetVariabilityType() {
		VariabilityType oldVariabilityType = variabilityType;
		boolean oldVariabilityTypeESet = variabilityTypeESet;
		variabilityType = VARIABILITY_TYPE_EDEFAULT;
		variabilityTypeESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, UmaPackage.ACTIVITY__VARIABILITY_TYPE, oldVariabilityType, VARIABILITY_TYPE_EDEFAULT, oldVariabilityTypeESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetVariabilityType() {
		return variabilityTypeESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case UmaPackage.ACTIVITY__GROUP2:
				return ((InternalEList)getGroup2()).basicRemove(otherEnd, msgs);
			case UmaPackage.ACTIVITY__BREAKDOWN_ELEMENT:
				return ((InternalEList)getBreakdownElement()).basicRemove(otherEnd, msgs);
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
			case UmaPackage.ACTIVITY__PRECONDITION:
				return getPrecondition();
			case UmaPackage.ACTIVITY__POSTCONDITION:
				return getPostcondition();
			case UmaPackage.ACTIVITY__GROUP2:
				if (coreType) return getGroup2();
				return ((FeatureMap.Internal)getGroup2()).getWrapper();
			case UmaPackage.ACTIVITY__BREAKDOWN_ELEMENT:
				return getBreakdownElement();
			case UmaPackage.ACTIVITY__CHECKLIST:
				return getChecklist();
			case UmaPackage.ACTIVITY__CONCEPT:
				return getConcept();
			case UmaPackage.ACTIVITY__EXAMPLE:
				return getExample();
			case UmaPackage.ACTIVITY__GUIDELINE:
				return getGuideline();
			case UmaPackage.ACTIVITY__ROADMAP:
				return getRoadmap();
			case UmaPackage.ACTIVITY__REUSABLE_ASSET:
				return getReusableAsset();
			case UmaPackage.ACTIVITY__SUPPORTING_MATERIAL:
				return getSupportingMaterial();
			case UmaPackage.ACTIVITY__WHITEPAPER:
				return getWhitepaper();
			case UmaPackage.ACTIVITY__IS_ENACTABLE:
				return isIsEnactable() ? Boolean.TRUE : Boolean.FALSE;
			case UmaPackage.ACTIVITY__VARIABILITY_BASED_ON_ELEMENT:
				return getVariabilityBasedOnElement();
			case UmaPackage.ACTIVITY__VARIABILITY_TYPE:
				return getVariabilityType();
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
			case UmaPackage.ACTIVITY__PRECONDITION:
				setPrecondition((String)newValue);
				return;
			case UmaPackage.ACTIVITY__POSTCONDITION:
				setPostcondition((String)newValue);
				return;
			case UmaPackage.ACTIVITY__GROUP2:
				((FeatureMap.Internal)getGroup2()).set(newValue);
				return;
			case UmaPackage.ACTIVITY__BREAKDOWN_ELEMENT:
				getBreakdownElement().clear();
				getBreakdownElement().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__CHECKLIST:
				getChecklist().clear();
				getChecklist().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__CONCEPT:
				getConcept().clear();
				getConcept().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__EXAMPLE:
				getExample().clear();
				getExample().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__GUIDELINE:
				getGuideline().clear();
				getGuideline().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__ROADMAP:
				getRoadmap().clear();
				getRoadmap().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__REUSABLE_ASSET:
				getReusableAsset().clear();
				getReusableAsset().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__SUPPORTING_MATERIAL:
				getSupportingMaterial().clear();
				getSupportingMaterial().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__WHITEPAPER:
				getWhitepaper().clear();
				getWhitepaper().addAll((Collection)newValue);
				return;
			case UmaPackage.ACTIVITY__IS_ENACTABLE:
				setIsEnactable(((Boolean)newValue).booleanValue());
				return;
			case UmaPackage.ACTIVITY__VARIABILITY_BASED_ON_ELEMENT:
				setVariabilityBasedOnElement((String)newValue);
				return;
			case UmaPackage.ACTIVITY__VARIABILITY_TYPE:
				setVariabilityType((VariabilityType)newValue);
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
			case UmaPackage.ACTIVITY__PRECONDITION:
				setPrecondition(PRECONDITION_EDEFAULT);
				return;
			case UmaPackage.ACTIVITY__POSTCONDITION:
				setPostcondition(POSTCONDITION_EDEFAULT);
				return;
			case UmaPackage.ACTIVITY__GROUP2:
				getGroup2().clear();
				return;
			case UmaPackage.ACTIVITY__BREAKDOWN_ELEMENT:
				getBreakdownElement().clear();
				return;
			case UmaPackage.ACTIVITY__CHECKLIST:
				getChecklist().clear();
				return;
			case UmaPackage.ACTIVITY__CONCEPT:
				getConcept().clear();
				return;
			case UmaPackage.ACTIVITY__EXAMPLE:
				getExample().clear();
				return;
			case UmaPackage.ACTIVITY__GUIDELINE:
				getGuideline().clear();
				return;
			case UmaPackage.ACTIVITY__ROADMAP:
				getRoadmap().clear();
				return;
			case UmaPackage.ACTIVITY__REUSABLE_ASSET:
				getReusableAsset().clear();
				return;
			case UmaPackage.ACTIVITY__SUPPORTING_MATERIAL:
				getSupportingMaterial().clear();
				return;
			case UmaPackage.ACTIVITY__WHITEPAPER:
				getWhitepaper().clear();
				return;
			case UmaPackage.ACTIVITY__IS_ENACTABLE:
				unsetIsEnactable();
				return;
			case UmaPackage.ACTIVITY__VARIABILITY_BASED_ON_ELEMENT:
				setVariabilityBasedOnElement(VARIABILITY_BASED_ON_ELEMENT_EDEFAULT);
				return;
			case UmaPackage.ACTIVITY__VARIABILITY_TYPE:
				unsetVariabilityType();
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
			case UmaPackage.ACTIVITY__PRECONDITION:
				return PRECONDITION_EDEFAULT == null ? precondition != null : !PRECONDITION_EDEFAULT.equals(precondition);
			case UmaPackage.ACTIVITY__POSTCONDITION:
				return POSTCONDITION_EDEFAULT == null ? postcondition != null : !POSTCONDITION_EDEFAULT.equals(postcondition);
			case UmaPackage.ACTIVITY__GROUP2:
				return group2 != null && !group2.isEmpty();
			case UmaPackage.ACTIVITY__BREAKDOWN_ELEMENT:
				return !getBreakdownElement().isEmpty();
			case UmaPackage.ACTIVITY__CHECKLIST:
				return !getChecklist().isEmpty();
			case UmaPackage.ACTIVITY__CONCEPT:
				return !getConcept().isEmpty();
			case UmaPackage.ACTIVITY__EXAMPLE:
				return !getExample().isEmpty();
			case UmaPackage.ACTIVITY__GUIDELINE:
				return !getGuideline().isEmpty();
			case UmaPackage.ACTIVITY__ROADMAP:
				return !getRoadmap().isEmpty();
			case UmaPackage.ACTIVITY__REUSABLE_ASSET:
				return !getReusableAsset().isEmpty();
			case UmaPackage.ACTIVITY__SUPPORTING_MATERIAL:
				return !getSupportingMaterial().isEmpty();
			case UmaPackage.ACTIVITY__WHITEPAPER:
				return !getWhitepaper().isEmpty();
			case UmaPackage.ACTIVITY__IS_ENACTABLE:
				return isSetIsEnactable();
			case UmaPackage.ACTIVITY__VARIABILITY_BASED_ON_ELEMENT:
				return VARIABILITY_BASED_ON_ELEMENT_EDEFAULT == null ? variabilityBasedOnElement != null : !VARIABILITY_BASED_ON_ELEMENT_EDEFAULT.equals(variabilityBasedOnElement);
			case UmaPackage.ACTIVITY__VARIABILITY_TYPE:
				return isSetVariabilityType();
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
		result.append(" (precondition: ");
		result.append(precondition);
		result.append(", postcondition: ");
		result.append(postcondition);
		result.append(", group2: ");
		result.append(group2);
		result.append(", isEnactable: ");
		if (isEnactableESet) result.append(isEnactable); else result.append("<unset>");
		result.append(", variabilityBasedOnElement: ");
		result.append(variabilityBasedOnElement);
		result.append(", variabilityType: ");
		if (variabilityTypeESet) result.append(variabilityType); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //ActivityImpl