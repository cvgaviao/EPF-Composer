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
 * A representation of the model object '<em><b>Property</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Property#getKey <em>Key</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Property#getValue <em>Value</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getProperty()
 * @model
 * @generated
 */
public interface Property extends DiagramElement {
	/**
	 * Returns the value of the '<em><b>Key</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Key</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Key</em>' attribute.
	 * @see #isSetKey()
	 * @see #unsetKey()
	 * @see #setKey(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getProperty_Key()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getKey();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Property#getKey <em>Key</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Key</em>' attribute.
	 * @see #isSetKey()
	 * @see #unsetKey()
	 * @see #getKey()
	 * @generated
	 */
	void setKey(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Property#getKey <em>Key</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetKey()
	 * @see #getKey()
	 * @see #setKey(String)
	 * @generated
	 */
	void unsetKey();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Property#getKey <em>Key</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Key</em>' attribute is set.
	 * @see #unsetKey()
	 * @see #getKey()
	 * @see #setKey(String)
	 * @generated
	 */
	boolean isSetKey();

	/**
	 * Returns the value of the '<em><b>Value</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Value</em>' attribute.
	 * @see #isSetValue()
	 * @see #unsetValue()
	 * @see #setValue(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getProperty_Value()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getValue();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Property#getValue <em>Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Value</em>' attribute.
	 * @see #isSetValue()
	 * @see #unsetValue()
	 * @see #getValue()
	 * @generated
	 */
	void setValue(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Property#getValue <em>Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetValue()
	 * @see #getValue()
	 * @see #setValue(String)
	 * @generated
	 */
	void unsetValue();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Property#getValue <em>Value</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Value</em>' attribute is set.
	 * @see #unsetValue()
	 * @see #getValue()
	 * @see #setValue(String)
	 * @generated
	 */
	boolean isSetValue();

} // Property
