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

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Kind</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Kind#getApplicableMetaClass <em>Applicable Meta Class</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Kind#getIsPrimaryKind <em>Is Primary Kind</em>}</li>
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
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Applicable Meta Class</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Applicable Meta Class</em>' attribute.
	 * @see #isSetApplicableMetaClass()
	 * @see #unsetApplicableMetaClass()
	 * @see #setApplicableMetaClass(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getKind_ApplicableMetaClass()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" required="true" ordered="false"
	 * @generated
	 */
	String getApplicableMetaClass();

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
	void setApplicableMetaClass(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Kind#getApplicableMetaClass <em>Applicable Meta Class</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetApplicableMetaClass()
	 * @see #getApplicableMetaClass()
	 * @see #setApplicableMetaClass(String)
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
	 * @see #setApplicableMetaClass(String)
	 * @generated
	 */
	boolean isSetApplicableMetaClass();

	/**
	 * Returns the value of the '<em><b>Is Primary Kind</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Is Primary Kind</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Primary Kind</em>' attribute.
	 * @see #isSetIsPrimaryKind()
	 * @see #unsetIsPrimaryKind()
	 * @see #setIsPrimaryKind(Boolean)
	 * @see org.eclipse.epf.uma.UmaPackage#getKind_IsPrimaryKind()
	 * @model default="false" unsettable="true" dataType="org.eclipse.epf.uma.Boolean" required="true" ordered="false"
	 * @generated
	 */
	Boolean getIsPrimaryKind();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Kind#getIsPrimaryKind <em>Is Primary Kind</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Primary Kind</em>' attribute.
	 * @see #isSetIsPrimaryKind()
	 * @see #unsetIsPrimaryKind()
	 * @see #getIsPrimaryKind()
	 * @generated
	 */
	void setIsPrimaryKind(Boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Kind#getIsPrimaryKind <em>Is Primary Kind</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetIsPrimaryKind()
	 * @see #getIsPrimaryKind()
	 * @see #setIsPrimaryKind(Boolean)
	 * @generated
	 */
	void unsetIsPrimaryKind();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Kind#getIsPrimaryKind <em>Is Primary Kind</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Is Primary Kind</em>' attribute is set.
	 * @see #unsetIsPrimaryKind()
	 * @see #getIsPrimaryKind()
	 * @see #setIsPrimaryKind(Boolean)
	 * @generated
	 */
	boolean isSetIsPrimaryKind();

} // Kind
