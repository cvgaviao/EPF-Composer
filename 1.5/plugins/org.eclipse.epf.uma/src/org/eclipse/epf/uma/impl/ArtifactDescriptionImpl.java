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
import org.eclipse.epf.uma.ArtifactDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Artifact Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.ArtifactDescriptionImpl#getBriefOutline <em>Brief Outline</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ArtifactDescriptionImpl#getRepresentationOptions <em>Representation Options</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ArtifactDescriptionImpl#getRepresentation <em>Representation</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.ArtifactDescriptionImpl#getNotation <em>Notation</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ArtifactDescriptionImpl extends WorkProductDescriptionImpl
		implements ArtifactDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getBriefOutline() <em>Brief Outline</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBriefOutline()
	 * @generated
	 * @ordered
	 */
	protected static final String BRIEF_OUTLINE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getBriefOutline() <em>Brief Outline</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBriefOutline()
	 * @generated
	 * @ordered
	 */
	protected String briefOutline = BRIEF_OUTLINE_EDEFAULT;

	/**
	 * This is true if the Brief Outline attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean briefOutlineESet;

	/**
	 * The default value of the '{@link #getRepresentationOptions() <em>Representation Options</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRepresentationOptions()
	 * @generated
	 * @ordered
	 */
	protected static final String REPRESENTATION_OPTIONS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getRepresentationOptions() <em>Representation Options</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRepresentationOptions()
	 * @generated
	 * @ordered
	 */
	protected String representationOptions = REPRESENTATION_OPTIONS_EDEFAULT;

	/**
	 * This is true if the Representation Options attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean representationOptionsESet;

	/**
	 * The default value of the '{@link #getRepresentation() <em>Representation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRepresentation()
	 * @generated
	 * @ordered
	 */
	protected static final String REPRESENTATION_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getRepresentation() <em>Representation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRepresentation()
	 * @generated
	 * @ordered
	 */
	protected String representation = REPRESENTATION_EDEFAULT;

	/**
	 * This is true if the Representation attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean representationESet;

	/**
	 * The default value of the '{@link #getNotation() <em>Notation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNotation()
	 * @generated
	 * @ordered
	 */
	protected static final String NOTATION_EDEFAULT = ""; //$NON-NLS-1$

	/**
	 * The cached value of the '{@link #getNotation() <em>Notation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNotation()
	 * @generated
	 * @ordered
	 */
	protected String notation = NOTATION_EDEFAULT;

	/**
	 * This is true if the Notation attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean notationESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ArtifactDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.ARTIFACT_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getBriefOutline() {
		return briefOutline;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBriefOutline(String newBriefOutline) {
		String oldBriefOutline = briefOutline;
		briefOutline = newBriefOutline;
		boolean oldBriefOutlineESet = briefOutlineESet;
		briefOutlineESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ARTIFACT_DESCRIPTION__BRIEF_OUTLINE,
					oldBriefOutline, briefOutline, !oldBriefOutlineESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetBriefOutline() {
		String oldBriefOutline = briefOutline;
		boolean oldBriefOutlineESet = briefOutlineESet;
		briefOutline = BRIEF_OUTLINE_EDEFAULT;
		briefOutlineESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ARTIFACT_DESCRIPTION__BRIEF_OUTLINE,
					oldBriefOutline, BRIEF_OUTLINE_EDEFAULT,
					oldBriefOutlineESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetBriefOutline() {
		return briefOutlineESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getRepresentationOptions() {
		return representationOptions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRepresentationOptions(String newRepresentationOptions) {
		String oldRepresentationOptions = representationOptions;
		representationOptions = newRepresentationOptions;
		boolean oldRepresentationOptionsESet = representationOptionsESet;
		representationOptionsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION_OPTIONS,
					oldRepresentationOptions, representationOptions,
					!oldRepresentationOptionsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRepresentationOptions() {
		String oldRepresentationOptions = representationOptions;
		boolean oldRepresentationOptionsESet = representationOptionsESet;
		representationOptions = REPRESENTATION_OPTIONS_EDEFAULT;
		representationOptionsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION_OPTIONS,
					oldRepresentationOptions, REPRESENTATION_OPTIONS_EDEFAULT,
					oldRepresentationOptionsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRepresentationOptions() {
		return representationOptionsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getRepresentation() {
		return representation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRepresentation(String newRepresentation) {
		String oldRepresentation = representation;
		representation = newRepresentation;
		boolean oldRepresentationESet = representationESet;
		representationESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION,
					oldRepresentation, representation, !oldRepresentationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRepresentation() {
		String oldRepresentation = representation;
		boolean oldRepresentationESet = representationESet;
		representation = REPRESENTATION_EDEFAULT;
		representationESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION,
					oldRepresentation, REPRESENTATION_EDEFAULT,
					oldRepresentationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRepresentation() {
		return representationESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getNotation() {
		return notation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setNotation(String newNotation) {
		String oldNotation = notation;
		notation = newNotation;
		boolean oldNotationESet = notationESet;
		notationESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ARTIFACT_DESCRIPTION__NOTATION, oldNotation,
					notation, !oldNotationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetNotation() {
		String oldNotation = notation;
		boolean oldNotationESet = notationESet;
		notation = NOTATION_EDEFAULT;
		notationESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ARTIFACT_DESCRIPTION__NOTATION, oldNotation,
					NOTATION_EDEFAULT, oldNotationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetNotation() {
		return notationESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.ARTIFACT_DESCRIPTION__BRIEF_OUTLINE:
			return getBriefOutline();
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION_OPTIONS:
			return getRepresentationOptions();
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION:
			return getRepresentation();
		case UmaPackage.ARTIFACT_DESCRIPTION__NOTATION:
			return getNotation();
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
		case UmaPackage.ARTIFACT_DESCRIPTION__BRIEF_OUTLINE:
			setBriefOutline((String) newValue);
			return;
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION_OPTIONS:
			setRepresentationOptions((String) newValue);
			return;
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION:
			setRepresentation((String) newValue);
			return;
		case UmaPackage.ARTIFACT_DESCRIPTION__NOTATION:
			setNotation((String) newValue);
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
		case UmaPackage.ARTIFACT_DESCRIPTION__BRIEF_OUTLINE:
			unsetBriefOutline();
			return;
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION_OPTIONS:
			unsetRepresentationOptions();
			return;
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION:
			unsetRepresentation();
			return;
		case UmaPackage.ARTIFACT_DESCRIPTION__NOTATION:
			unsetNotation();
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
		case UmaPackage.ARTIFACT_DESCRIPTION__BRIEF_OUTLINE:
			return isSetBriefOutline();
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION_OPTIONS:
			return isSetRepresentationOptions();
		case UmaPackage.ARTIFACT_DESCRIPTION__REPRESENTATION:
			return isSetRepresentation();
		case UmaPackage.ARTIFACT_DESCRIPTION__NOTATION:
			return isSetNotation();
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
		result.append(" (briefOutline: "); //$NON-NLS-1$
		if (briefOutlineESet)
			result.append(briefOutline);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", representationOptions: "); //$NON-NLS-1$
		if (representationOptionsESet)
			result.append(representationOptions);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", representation: "); //$NON-NLS-1$
		if (representationESet)
			result.append(representation);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", notation: "); //$NON-NLS-1$
		if (notationESet)
			result.append(notation);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //ArtifactDescriptionImpl
