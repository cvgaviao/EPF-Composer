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

import java.util.Date;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.MethodUnit;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Method Unit</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodUnitImpl#getAuthors <em>Authors</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodUnitImpl#getChangeDate <em>Change Date</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodUnitImpl#getChangeDescription <em>Change Description</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodUnitImpl#getVersion <em>Version</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.MethodUnitImpl#getCopyrightStatement <em>Copyright Statement</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class MethodUnitImpl extends MethodElementImpl implements
		MethodUnit {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getAuthors() <em>Authors</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAuthors()
	 * @generated
	 * @ordered
	 */
	protected static final String AUTHORS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getAuthors() <em>Authors</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAuthors()
	 * @generated
	 * @ordered
	 */
	protected String authors = AUTHORS_EDEFAULT;

	/**
	 * This is true if the Authors attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean authorsESet;

	/**
	 * The default value of the '{@link #getChangeDate() <em>Change Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChangeDate()
	 * @generated
	 * @ordered
	 */
	protected static final Date CHANGE_DATE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getChangeDate() <em>Change Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChangeDate()
	 * @generated
	 * @ordered
	 */
	protected Date changeDate = CHANGE_DATE_EDEFAULT;

	/**
	 * This is true if the Change Date attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean changeDateESet;

	/**
	 * The default value of the '{@link #getChangeDescription() <em>Change Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChangeDescription()
	 * @generated
	 * @ordered
	 */
	protected static final String CHANGE_DESCRIPTION_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getChangeDescription() <em>Change Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChangeDescription()
	 * @generated
	 * @ordered
	 */
	protected String changeDescription = CHANGE_DESCRIPTION_EDEFAULT;

	/**
	 * This is true if the Change Description attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean changeDescriptionESet;

	/**
	 * The default value of the '{@link #getVersion() <em>Version</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVersion()
	 * @generated
	 * @ordered
	 */
	protected static final String VERSION_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getVersion() <em>Version</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVersion()
	 * @generated
	 * @ordered
	 */
	protected String version = VERSION_EDEFAULT;

	/**
	 * This is true if the Version attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean versionESet;

	/**
	 * The cached value of the '{@link #getCopyrightStatement() <em>Copyright Statement</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCopyrightStatement()
	 * @generated
	 * @ordered
	 */
	protected SupportingMaterial copyrightStatement;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected MethodUnitImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.METHOD_UNIT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAuthors() {
		return authors;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAuthors(String newAuthors) {
		String oldAuthors = authors;
		authors = newAuthors;
		boolean oldAuthorsESet = authorsESet;
		authorsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_UNIT__AUTHORS, oldAuthors, authors,
					!oldAuthorsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetAuthors() {
		String oldAuthors = authors;
		boolean oldAuthorsESet = authorsESet;
		authors = AUTHORS_EDEFAULT;
		authorsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_UNIT__AUTHORS, oldAuthors,
					AUTHORS_EDEFAULT, oldAuthorsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetAuthors() {
		return authorsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Date getChangeDate() {
		return changeDate;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setChangeDate(Date newChangeDate) {
		Date oldChangeDate = changeDate;
		changeDate = newChangeDate;
		boolean oldChangeDateESet = changeDateESet;
		changeDateESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_UNIT__CHANGE_DATE, oldChangeDate,
					changeDate, !oldChangeDateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetChangeDate() {
		Date oldChangeDate = changeDate;
		boolean oldChangeDateESet = changeDateESet;
		changeDate = CHANGE_DATE_EDEFAULT;
		changeDateESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_UNIT__CHANGE_DATE, oldChangeDate,
					CHANGE_DATE_EDEFAULT, oldChangeDateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetChangeDate() {
		return changeDateESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getChangeDescription() {
		return changeDescription;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setChangeDescription(String newChangeDescription) {
		String oldChangeDescription = changeDescription;
		changeDescription = newChangeDescription;
		boolean oldChangeDescriptionESet = changeDescriptionESet;
		changeDescriptionESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_UNIT__CHANGE_DESCRIPTION,
					oldChangeDescription, changeDescription,
					!oldChangeDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetChangeDescription() {
		String oldChangeDescription = changeDescription;
		boolean oldChangeDescriptionESet = changeDescriptionESet;
		changeDescription = CHANGE_DESCRIPTION_EDEFAULT;
		changeDescriptionESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_UNIT__CHANGE_DESCRIPTION,
					oldChangeDescription, CHANGE_DESCRIPTION_EDEFAULT,
					oldChangeDescriptionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetChangeDescription() {
		return changeDescriptionESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getVersion() {
		return version;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVersion(String newVersion) {
		String oldVersion = version;
		version = newVersion;
		boolean oldVersionESet = versionESet;
		versionESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_UNIT__VERSION, oldVersion, version,
					!oldVersionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetVersion() {
		String oldVersion = version;
		boolean oldVersionESet = versionESet;
		version = VERSION_EDEFAULT;
		versionESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.METHOD_UNIT__VERSION, oldVersion,
					VERSION_EDEFAULT, oldVersionESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetVersion() {
		return versionESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SupportingMaterial getCopyrightStatement() {
		if (copyrightStatement != null
				&& ((EObject) copyrightStatement).eIsProxy()) {
			InternalEObject oldCopyrightStatement = (InternalEObject) copyrightStatement;
			copyrightStatement = (SupportingMaterial) eResolveProxy(oldCopyrightStatement);
			if (copyrightStatement != oldCopyrightStatement) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.METHOD_UNIT__COPYRIGHT_STATEMENT,
							oldCopyrightStatement, copyrightStatement));
			}
		}
		return copyrightStatement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SupportingMaterial basicGetCopyrightStatement() {
		return copyrightStatement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCopyrightStatement(SupportingMaterial newCopyrightStatement) {
		SupportingMaterial oldCopyrightStatement = copyrightStatement;
		copyrightStatement = newCopyrightStatement;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.METHOD_UNIT__COPYRIGHT_STATEMENT,
					oldCopyrightStatement, copyrightStatement));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.METHOD_UNIT__AUTHORS:
			return getAuthors();
		case UmaPackage.METHOD_UNIT__CHANGE_DATE:
			return getChangeDate();
		case UmaPackage.METHOD_UNIT__CHANGE_DESCRIPTION:
			return getChangeDescription();
		case UmaPackage.METHOD_UNIT__VERSION:
			return getVersion();
		case UmaPackage.METHOD_UNIT__COPYRIGHT_STATEMENT:
			if (resolve)
				return getCopyrightStatement();
			return basicGetCopyrightStatement();
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
		case UmaPackage.METHOD_UNIT__AUTHORS:
			setAuthors((String) newValue);
			return;
		case UmaPackage.METHOD_UNIT__CHANGE_DATE:
			setChangeDate((Date) newValue);
			return;
		case UmaPackage.METHOD_UNIT__CHANGE_DESCRIPTION:
			setChangeDescription((String) newValue);
			return;
		case UmaPackage.METHOD_UNIT__VERSION:
			setVersion((String) newValue);
			return;
		case UmaPackage.METHOD_UNIT__COPYRIGHT_STATEMENT:
			setCopyrightStatement((SupportingMaterial) newValue);
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
		case UmaPackage.METHOD_UNIT__AUTHORS:
			unsetAuthors();
			return;
		case UmaPackage.METHOD_UNIT__CHANGE_DATE:
			unsetChangeDate();
			return;
		case UmaPackage.METHOD_UNIT__CHANGE_DESCRIPTION:
			unsetChangeDescription();
			return;
		case UmaPackage.METHOD_UNIT__VERSION:
			unsetVersion();
			return;
		case UmaPackage.METHOD_UNIT__COPYRIGHT_STATEMENT:
			setCopyrightStatement((SupportingMaterial) null);
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
		case UmaPackage.METHOD_UNIT__AUTHORS:
			return isSetAuthors();
		case UmaPackage.METHOD_UNIT__CHANGE_DATE:
			return isSetChangeDate();
		case UmaPackage.METHOD_UNIT__CHANGE_DESCRIPTION:
			return isSetChangeDescription();
		case UmaPackage.METHOD_UNIT__VERSION:
			return isSetVersion();
		case UmaPackage.METHOD_UNIT__COPYRIGHT_STATEMENT:
			return copyrightStatement != null;
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
		result.append(" (authors: "); //$NON-NLS-1$
		if (authorsESet)
			result.append(authors);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", changeDate: "); //$NON-NLS-1$
		if (changeDateESet)
			result.append(changeDate);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", changeDescription: "); //$NON-NLS-1$
		if (changeDescriptionESet)
			result.append(changeDescription);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", version: "); //$NON-NLS-1$
		if (versionESet)
			result.append(version);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //MethodUnitImpl
