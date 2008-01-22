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
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.uma.ContentDescription;
import org.eclipse.epf.uma.Section;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Content Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.ContentDescriptionImpl#getMainDescription <em>Main Description</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ContentDescriptionImpl#getSections <em>Sections</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ContentDescriptionImpl#getExternalId <em>External Id</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ContentDescriptionImpl#getKeyConsiderations <em>Key Considerations</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ContentDescriptionImpl extends MethodUnitImpl implements
		ContentDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getMainDescription() <em>Main Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMainDescription()
	 * @generated
	 * @ordered
	 */
	protected static final String MAIN_DESCRIPTION_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getMainDescription() <em>Main Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMainDescription()
	 * @generated
	 * @ordered
	 */
	protected String mainDescription = MAIN_DESCRIPTION_EDEFAULT;

	/**
	 * This is true if the Main Description attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean mainDescriptionESet;

	/**
	 * The cached value of the '{@link #getSections() <em>Sections</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSections()
	 * @generated
	 * @ordered
	 */
	protected EList<Section> sections;

	/**
	 * The default value of the '{@link #getExternalId() <em>External Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalId()
	 * @generated
	 * @ordered
	 */
	protected static final String EXTERNAL_ID_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getExternalId() <em>External Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalId()
	 * @generated
	 * @ordered
	 */
	protected String externalId = EXTERNAL_ID_EDEFAULT;

	/**
	 * This is true if the External Id attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean externalIdESet;

	/**
	 * The default value of the '{@link #getKeyConsiderations() <em>Key Considerations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKeyConsiderations()
	 * @generated
	 * @ordered
	 */
	protected static final String KEY_CONSIDERATIONS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getKeyConsiderations() <em>Key Considerations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKeyConsiderations()
	 * @generated
	 * @ordered
	 */
	protected String keyConsiderations = KEY_CONSIDERATIONS_EDEFAULT;

	/**
	 * This is true if the Key Considerations attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean keyConsiderationsESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ContentDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.CONTENT_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getMainDescription() {
		return mainDescription;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMainDescription(String newMainDescription) {
		String oldMainDescription = mainDescription;
		mainDescription = newMainDescription;
		boolean oldMainDescriptionESet = mainDescriptionESet;
		mainDescriptionESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.CONTENT_DESCRIPTION__MAIN_DESCRIPTION,
					oldMainDescription, mainDescription,
					!oldMainDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetMainDescription() {
		String oldMainDescription = mainDescription;
		boolean oldMainDescriptionESet = mainDescriptionESet;
		mainDescription = MAIN_DESCRIPTION_EDEFAULT;
		mainDescriptionESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.CONTENT_DESCRIPTION__MAIN_DESCRIPTION,
					oldMainDescription, MAIN_DESCRIPTION_EDEFAULT,
					oldMainDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetMainDescription() {
		return mainDescriptionESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List<Section> getSections() {
		if (sections == null) {
			sections = new EObjectContainmentEList.Resolving<Section>(
					Section.class, this,
					UmaPackage.CONTENT_DESCRIPTION__SECTIONS);
		}
		return sections;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getExternalId() {
		return externalId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setExternalId(String newExternalId) {
		String oldExternalId = externalId;
		externalId = newExternalId;
		boolean oldExternalIdESet = externalIdESet;
		externalIdESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.CONTENT_DESCRIPTION__EXTERNAL_ID, oldExternalId,
					externalId, !oldExternalIdESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetExternalId() {
		String oldExternalId = externalId;
		boolean oldExternalIdESet = externalIdESet;
		externalId = EXTERNAL_ID_EDEFAULT;
		externalIdESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.CONTENT_DESCRIPTION__EXTERNAL_ID, oldExternalId,
					EXTERNAL_ID_EDEFAULT, oldExternalIdESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetExternalId() {
		return externalIdESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getKeyConsiderations() {
		return keyConsiderations;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setKeyConsiderations(String newKeyConsiderations) {
		String oldKeyConsiderations = keyConsiderations;
		keyConsiderations = newKeyConsiderations;
		boolean oldKeyConsiderationsESet = keyConsiderationsESet;
		keyConsiderationsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.CONTENT_DESCRIPTION__KEY_CONSIDERATIONS,
					oldKeyConsiderations, keyConsiderations,
					!oldKeyConsiderationsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetKeyConsiderations() {
		String oldKeyConsiderations = keyConsiderations;
		boolean oldKeyConsiderationsESet = keyConsiderationsESet;
		keyConsiderations = KEY_CONSIDERATIONS_EDEFAULT;
		keyConsiderationsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.CONTENT_DESCRIPTION__KEY_CONSIDERATIONS,
					oldKeyConsiderations, KEY_CONSIDERATIONS_EDEFAULT,
					oldKeyConsiderationsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetKeyConsiderations() {
		return keyConsiderationsESet;
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
		case UmaPackage.CONTENT_DESCRIPTION__SECTIONS:
			return ((InternalEList<?>) getSections()).basicRemove(otherEnd,
					msgs);
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
		case UmaPackage.CONTENT_DESCRIPTION__MAIN_DESCRIPTION:
			return getMainDescription();
		case UmaPackage.CONTENT_DESCRIPTION__SECTIONS:
			return getSections();
		case UmaPackage.CONTENT_DESCRIPTION__EXTERNAL_ID:
			return getExternalId();
		case UmaPackage.CONTENT_DESCRIPTION__KEY_CONSIDERATIONS:
			return getKeyConsiderations();
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
		case UmaPackage.CONTENT_DESCRIPTION__MAIN_DESCRIPTION:
			setMainDescription((String) newValue);
			return;
		case UmaPackage.CONTENT_DESCRIPTION__SECTIONS:
			getSections().clear();
			getSections().addAll((Collection<? extends Section>) newValue);
			return;
		case UmaPackage.CONTENT_DESCRIPTION__EXTERNAL_ID:
			setExternalId((String) newValue);
			return;
		case UmaPackage.CONTENT_DESCRIPTION__KEY_CONSIDERATIONS:
			setKeyConsiderations((String) newValue);
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
		case UmaPackage.CONTENT_DESCRIPTION__MAIN_DESCRIPTION:
			unsetMainDescription();
			return;
		case UmaPackage.CONTENT_DESCRIPTION__SECTIONS:
			getSections().clear();
			return;
		case UmaPackage.CONTENT_DESCRIPTION__EXTERNAL_ID:
			unsetExternalId();
			return;
		case UmaPackage.CONTENT_DESCRIPTION__KEY_CONSIDERATIONS:
			unsetKeyConsiderations();
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
		case UmaPackage.CONTENT_DESCRIPTION__MAIN_DESCRIPTION:
			return isSetMainDescription();
		case UmaPackage.CONTENT_DESCRIPTION__SECTIONS:
			return sections != null && !sections.isEmpty();
		case UmaPackage.CONTENT_DESCRIPTION__EXTERNAL_ID:
			return isSetExternalId();
		case UmaPackage.CONTENT_DESCRIPTION__KEY_CONSIDERATIONS:
			return isSetKeyConsiderations();
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
		result.append(" (mainDescription: "); //$NON-NLS-1$
		if (mainDescriptionESet)
			result.append(mainDescription);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", externalId: "); //$NON-NLS-1$
		if (externalIdESet)
			result.append(externalId);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", keyConsiderations: "); //$NON-NLS-1$
		if (keyConsiderationsESet)
			result.append(keyConsiderations);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //ContentDescriptionImpl
