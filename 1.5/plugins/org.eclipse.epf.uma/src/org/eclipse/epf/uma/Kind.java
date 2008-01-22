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
package org.eclipse.epf.uma;

import java.util.Set;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Kind</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Kind#getApplicableMetaClass <em>Applicable Meta Class</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getKind()
 * @model
 * @generated
 */
public interface Kind extends ContentElement {
	/**
	 * Returns the value of the '<em><b>Applicable Meta Class</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Applicable Meta Class</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Applicable Meta Class</em>' attribute.
	 * @see #isSetApplicableMetaClass()
	 * @see #unsetApplicableMetaClass()
	 * @see #setApplicableMetaClass(Set)
	 * @see org.eclipse.epf.uma.UmaPackage#getKind_ApplicableMetaClass()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Set" required="true" ordered="false"
	 * @generated
	 */
	Set getApplicableMetaClass();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Kind#getApplicableMetaClass <em>Applicable Meta Class</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Applicable Meta Class</em>' attribute.
	 * @see #isSetApplicableMetaClass()
	 * @see #unsetApplicableMetaClass()
	 * @see #getApplicableMetaClass()
	 * @generated
	 */
	void setApplicableMetaClass(Set value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Kind#getApplicableMetaClass <em>Applicable Meta Class</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetApplicableMetaClass()
	 * @see #getApplicableMetaClass()
	 * @see #setApplicableMetaClass(Set)
	 * @generated
	 */
	void unsetApplicableMetaClass();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Kind#getApplicableMetaClass <em>Applicable Meta Class</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Applicable Meta Class</em>' attribute is set.
	 * @see #unsetApplicableMetaClass()
	 * @see #getApplicableMetaClass()
	 * @see #setApplicableMetaClass(Set)
	 * @generated
	 */
	boolean isSetApplicableMetaClass();

} // Kind
