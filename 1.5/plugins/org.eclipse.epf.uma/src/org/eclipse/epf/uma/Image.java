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

import java.net.URI;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Image</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Image#getUri <em>Uri</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Image#getMimeType <em>Mime Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getImage()
 * @model
 * @generated
 */
public interface Image extends LeafElement {
	/**
	 * Returns the value of the '<em><b>Uri</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Uri</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Uri</em>' attribute.
	 * @see #isSetUri()
	 * @see #unsetUri()
	 * @see #setUri(URI)
	 * @see org.eclipse.epf.uma.UmaPackage#getImage_Uri()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.Uri" ordered="false"
	 * @generated
	 */
	URI getUri();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Image#getUri <em>Uri</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Uri</em>' attribute.
	 * @see #isSetUri()
	 * @see #unsetUri()
	 * @see #getUri()
	 * @generated
	 */
	void setUri(URI value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Image#getUri <em>Uri</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetUri()
	 * @see #getUri()
	 * @see #setUri(URI)
	 * @generated
	 */
	void unsetUri();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Image#getUri <em>Uri</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Uri</em>' attribute is set.
	 * @see #unsetUri()
	 * @see #getUri()
	 * @see #setUri(URI)
	 * @generated
	 */
	boolean isSetUri();

	/**
	 * Returns the value of the '<em><b>Mime Type</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Mime Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Mime Type</em>' attribute.
	 * @see #isSetMimeType()
	 * @see #unsetMimeType()
	 * @see #setMimeType(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getImage_MimeType()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getMimeType();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Image#getMimeType <em>Mime Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Mime Type</em>' attribute.
	 * @see #isSetMimeType()
	 * @see #unsetMimeType()
	 * @see #getMimeType()
	 * @generated
	 */
	void setMimeType(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Image#getMimeType <em>Mime Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetMimeType()
	 * @see #getMimeType()
	 * @see #setMimeType(String)
	 * @generated
	 */
	void unsetMimeType();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Image#getMimeType <em>Mime Type</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Mime Type</em>' attribute is set.
	 * @see #unsetMimeType()
	 * @see #getMimeType()
	 * @see #setMimeType(String)
	 * @generated
	 */
	boolean isSetMimeType();

} // Image
