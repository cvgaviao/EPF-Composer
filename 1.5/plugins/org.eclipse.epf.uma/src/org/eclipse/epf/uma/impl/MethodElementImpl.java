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
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.Kind;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Method Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getGuid <em>Guid</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getPresentationName <em>Presentation Name</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getBriefDescription <em>Brief Description</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getOwnedRules <em>Owned Rules</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getMethodElementProperty <em>Method Element Property</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getKind <em>Kind</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getSuppressed <em>Suppressed</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodElementImpl#getOrderingGuide <em>Ordering Guide</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class MethodElementImpl extends PackageableElementImpl
		implements MethodElement {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getGuid() <em>Guid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGuid()
	 * @generated
	 * @ordered
	 */
	protected static final String GUID_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getGuid() <em>Guid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGuid()
	 * @generated
	 * @ordered
	 */
	protected String guid = GUID_EDEFAULT;

	/**
	 * This is true if the Guid attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean guidESet;

	/**
	 * The default value of the '{@link #getPresentationName() <em>Presentation Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentationName()
	 * @generated
	 * @ordered
	 */
	protected static final String PRESENTATION_NAME_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getPresentationName() <em>Presentation Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentationName()
	 * @generated
	 * @ordered
	 */
	protected String presentationName = PRESENTATION_NAME_EDEFAULT;

	/**
	 * This is true if the Presentation Name attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean presentationNameESet;

	/**
	 * The default value of the '{@link #getBriefDescription() <em>Brief Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBriefDescription()
	 * @generated
	 * @ordered
	 */
	protected static final String BRIEF_DESCRIPTION_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getBriefDescription() <em>Brief Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBriefDescription()
	 * @generated
	 * @ordered
	 */
	protected String briefDescription = BRIEF_DESCRIPTION_EDEFAULT;

	/**
	 * This is true if the Brief Description attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean briefDescriptionESet;

	/**
	 * The cached value of the '{@link #getOwnedRules() <em>Owned Rules</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOwnedRules()
	 * @generated
	 * @ordered
	 */
	protected EList<Constraint> ownedRules;

	/**
	 * The cached value of the '{@link #getMethodElementProperty() <em>Method Element Property</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMethodElementProperty()
	 * @generated
	 * @ordered
	 */
	protected EList<MethodElementProperty> methodElementProperty;

	/**
	 * The cached value of the '{@link #getKind() <em>Kind</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKind()
	 * @generated
	 * @ordered
	 */
	protected EList<Kind> kind;

	/**
	 * The default value of the '{@link #getSuppressed() <em>Suppressed</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuppressed()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean SUPPRESSED_EDEFAULT = Boolean.FALSE;

	/**
	 * The cached value of the '{@link #getSuppressed() <em>Suppressed</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuppressed()
	 * @generated
	 * @ordered
	 */
	protected Boolean suppressed = SUPPRESSED_EDEFAULT;

	/**
	 * This is true if the Suppressed attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean suppressedESet;

	/**
	 * The default value of the '{@link #getOrderingGuide() <em>Ordering Guide</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOrderingGuide()
	 * @generated
	 * @ordered
	 */
	protected static final String ORDERING_GUIDE_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getOrderingGuide() <em>Ordering Guide</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOrderingGuide()
	 * @generated
	 * @ordered
	 */
	protected String orderingGuide = ORDERING_GUIDE_EDEFAULT;

	/**
	 * This is true if the Ordering Guide attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean orderingGuideESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected MethodElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.METHOD_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 */
	public String getGuid() {
		if (guid == null || guid.length() == 0) {
			guid = UmaUtil.generateGUID();
		}
		return guid;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 */
	public void setGuid(String newGuid) {
		String oldGuid = guid;
		guid = newGuid;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_ELEMENT__GUID, oldGuid, guid));

		// synch up the resource's IDToEObjectMap and EObjectToIDMap with newGuid
		//
		if (eResource() instanceof XMLResource) {
			XMLResource resource = ((XMLResource) eResource());
			resource.setID(this, newGuid);
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetGuid() {
		String oldGuid = guid;
		boolean oldGuidESet = guidESet;
		guid = GUID_EDEFAULT;
		guidESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_ELEMENT__GUID, oldGuid, GUID_EDEFAULT,
					oldGuidESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetGuid() {
		return guidESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPresentationName() {
		return presentationName;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPresentationName(String newPresentationName) {
		String oldPresentationName = presentationName;
		presentationName = newPresentationName;
		boolean oldPresentationNameESet = presentationNameESet;
		presentationNameESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_ELEMENT__PRESENTATION_NAME,
					oldPresentationName, presentationName,
					!oldPresentationNameESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetPresentationName() {
		String oldPresentationName = presentationName;
		boolean oldPresentationNameESet = presentationNameESet;
		presentationName = PRESENTATION_NAME_EDEFAULT;
		presentationNameESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_ELEMENT__PRESENTATION_NAME,
					oldPresentationName, PRESENTATION_NAME_EDEFAULT,
					oldPresentationNameESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetPresentationName() {
		return presentationNameESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getBriefDescription() {
		return briefDescription;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBriefDescription(String newBriefDescription) {
		String oldBriefDescription = briefDescription;
		briefDescription = newBriefDescription;
		boolean oldBriefDescriptionESet = briefDescriptionESet;
		briefDescriptionESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_ELEMENT__BRIEF_DESCRIPTION,
					oldBriefDescription, briefDescription,
					!oldBriefDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetBriefDescription() {
		String oldBriefDescription = briefDescription;
		boolean oldBriefDescriptionESet = briefDescriptionESet;
		briefDescription = BRIEF_DESCRIPTION_EDEFAULT;
		briefDescriptionESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_ELEMENT__BRIEF_DESCRIPTION,
					oldBriefDescription, BRIEF_DESCRIPTION_EDEFAULT,
					oldBriefDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetBriefDescription() {
		return briefDescriptionESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<Constraint> getOwnedRules() {
		if (ownedRules == null) {
			ownedRules = new EObjectContainmentEList.Resolving<Constraint>(
					Constraint.class, this,
					UmaPackage.METHOD_ELEMENT__OWNED_RULES);
		}
		return ownedRules;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<MethodElementProperty> getMethodElementProperty() {
		if (methodElementProperty == null) {
			methodElementProperty = new EObjectContainmentEList.Resolving<MethodElementProperty>(
					MethodElementProperty.class, this,
					UmaPackage.METHOD_ELEMENT__METHOD_ELEMENT_PROPERTY);
		}
		return methodElementProperty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<Kind> getKind() {
		if (kind == null) {
			kind = new EObjectResolvingEList<Kind>(Kind.class, this,
					UmaPackage.METHOD_ELEMENT__KIND);
		}
		return kind;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getSuppressed() {
		return suppressed;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSuppressed(Boolean newSuppressed) {
		Boolean oldSuppressed = suppressed;
		suppressed = newSuppressed;
		boolean oldSuppressedESet = suppressedESet;
		suppressedESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_ELEMENT__SUPPRESSED, oldSuppressed,
					suppressed, !oldSuppressedESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSuppressed() {
		Boolean oldSuppressed = suppressed;
		boolean oldSuppressedESet = suppressedESet;
		suppressed = SUPPRESSED_EDEFAULT;
		suppressedESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_ELEMENT__SUPPRESSED, oldSuppressed,
					SUPPRESSED_EDEFAULT, oldSuppressedESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSuppressed() {
		return suppressedESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getOrderingGuide() {
		return orderingGuide;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOrderingGuide(String newOrderingGuide) {
		String oldOrderingGuide = orderingGuide;
		orderingGuide = newOrderingGuide;
		boolean oldOrderingGuideESet = orderingGuideESet;
		orderingGuideESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_ELEMENT__ORDERING_GUIDE,
					oldOrderingGuide, orderingGuide, !oldOrderingGuideESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetOrderingGuide() {
		String oldOrderingGuide = orderingGuide;
		boolean oldOrderingGuideESet = orderingGuideESet;
		orderingGuide = ORDERING_GUIDE_EDEFAULT;
		orderingGuideESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_ELEMENT__ORDERING_GUIDE,
					oldOrderingGuide, ORDERING_GUIDE_EDEFAULT,
					oldOrderingGuideESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetOrderingGuide() {
		return orderingGuideESet;
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
		case UmaPackage.METHOD_ELEMENT__OWNED_RULES:
			return ((InternalEList<?>) getOwnedRules()).basicRemove(otherEnd,
					msgs);
		case UmaPackage.METHOD_ELEMENT__METHOD_ELEMENT_PROPERTY:
			return ((InternalEList<?>) getMethodElementProperty()).basicRemove(
					otherEnd, msgs);
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
		case UmaPackage.METHOD_ELEMENT__GUID:
			return getGuid();
		case UmaPackage.METHOD_ELEMENT__PRESENTATION_NAME:
			return getPresentationName();
		case UmaPackage.METHOD_ELEMENT__BRIEF_DESCRIPTION:
			return getBriefDescription();
		case UmaPackage.METHOD_ELEMENT__OWNED_RULES:
			return getOwnedRules();
		case UmaPackage.METHOD_ELEMENT__METHOD_ELEMENT_PROPERTY:
			return getMethodElementProperty();
		case UmaPackage.METHOD_ELEMENT__KIND:
			return getKind();
		case UmaPackage.METHOD_ELEMENT__SUPPRESSED:
			return getSuppressed();
		case UmaPackage.METHOD_ELEMENT__ORDERING_GUIDE:
			return getOrderingGuide();
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
		case UmaPackage.METHOD_ELEMENT__GUID:
			setGuid((String) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__PRESENTATION_NAME:
			setPresentationName((String) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__BRIEF_DESCRIPTION:
			setBriefDescription((String) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__OWNED_RULES:
			getOwnedRules().clear();
			getOwnedRules().addAll((Collection<? extends Constraint>) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__METHOD_ELEMENT_PROPERTY:
			getMethodElementProperty().clear();
			getMethodElementProperty().addAll(
					(Collection<? extends MethodElementProperty>) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__KIND:
			getKind().clear();
			getKind().addAll((Collection<? extends Kind>) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__SUPPRESSED:
			setSuppressed((Boolean) newValue);
			return;
		case UmaPackage.METHOD_ELEMENT__ORDERING_GUIDE:
			setOrderingGuide((String) newValue);
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
		case UmaPackage.METHOD_ELEMENT__GUID:
			unsetGuid();
			return;
		case UmaPackage.METHOD_ELEMENT__PRESENTATION_NAME:
			unsetPresentationName();
			return;
		case UmaPackage.METHOD_ELEMENT__BRIEF_DESCRIPTION:
			unsetBriefDescription();
			return;
		case UmaPackage.METHOD_ELEMENT__OWNED_RULES:
			getOwnedRules().clear();
			return;
		case UmaPackage.METHOD_ELEMENT__METHOD_ELEMENT_PROPERTY:
			getMethodElementProperty().clear();
			return;
		case UmaPackage.METHOD_ELEMENT__KIND:
			getKind().clear();
			return;
		case UmaPackage.METHOD_ELEMENT__SUPPRESSED:
			unsetSuppressed();
			return;
		case UmaPackage.METHOD_ELEMENT__ORDERING_GUIDE:
			unsetOrderingGuide();
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
		case UmaPackage.METHOD_ELEMENT__GUID:
			return isSetGuid();
		case UmaPackage.METHOD_ELEMENT__PRESENTATION_NAME:
			return isSetPresentationName();
		case UmaPackage.METHOD_ELEMENT__BRIEF_DESCRIPTION:
			return isSetBriefDescription();
		case UmaPackage.METHOD_ELEMENT__OWNED_RULES:
			return ownedRules != null && !ownedRules.isEmpty();
		case UmaPackage.METHOD_ELEMENT__METHOD_ELEMENT_PROPERTY:
			return methodElementProperty != null
					&& !methodElementProperty.isEmpty();
		case UmaPackage.METHOD_ELEMENT__KIND:
			return kind != null && !kind.isEmpty();
		case UmaPackage.METHOD_ELEMENT__SUPPRESSED:
			return isSetSuppressed();
		case UmaPackage.METHOD_ELEMENT__ORDERING_GUIDE:
			return isSetOrderingGuide();
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
		result.append(" (guid: "); //$NON-NLS-1$
		if (guidESet)
			result.append(guid);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", presentationName: "); //$NON-NLS-1$
		if (presentationNameESet)
			result.append(presentationName);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", briefDescription: "); //$NON-NLS-1$
		if (briefDescriptionESet)
			result.append(briefDescription);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", suppressed: "); //$NON-NLS-1$
		if (suppressedESet)
			result.append(suppressed);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", orderingGuide: "); //$NON-NLS-1$
		if (orderingGuideESet)
			result.append(orderingGuide);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //MethodElementImpl
