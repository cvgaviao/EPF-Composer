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

import java.util.List;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Content Description</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * Content Description is a Method Element that is used to store the textual description for a Content Element.  It defines standard attributes applicable for all Content Element types.  Specific Content Element sub-types can define their own matching Content Description sub-types. 
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.ContentDescription#getMainDescription <em>Main Description</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentDescription#getSections <em>Sections</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentDescription#getExternalId <em>External Id</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentDescription#getKeyConsiderations <em>Key Considerations</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getContentDescription()
 * @model
 * @generated
 */
public interface ContentDescription extends MethodUnit {
	/**
	 * Returns the value of the '<em><b>Main Description</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * This attribute store the main descriptive text for the Content Element.  All text that is not part of any of the more specific attributes shall be stored here.  If the Content Description is divided into sections using the Section class, then only the text from the 'start' of the content description to the first section will be stored here (similar to a normal document where you can place text between its beginning to its first diction heading).
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Main Description</em>' attribute.
	 * @see #isSetMainDescription()
	 * @see #unsetMainDescription()
	 * @see #setMainDescription(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getContentDescription_MainDescription()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getMainDescription();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.ContentDescription#getMainDescription <em>Main Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Main Description</em>' attribute.
	 * @see #isSetMainDescription()
	 * @see #unsetMainDescription()
	 * @see #getMainDescription()
	 * @generated
	 */
	void setMainDescription(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.ContentDescription#getMainDescription <em>Main Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetMainDescription()
	 * @see #getMainDescription()
	 * @see #setMainDescription(String)
	 * @generated
	 */
	void unsetMainDescription();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.ContentDescription#getMainDescription <em>Main Description</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Main Description</em>' attribute is set.
	 * @see #unsetMainDescription()
	 * @see #getMainDescription()
	 * @see #setMainDescription(String)
	 * @generated
	 */
	boolean isSetMainDescription();

	/**
	 * Returns the value of the '<em><b>Sections</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Section}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sections</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sections</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getContentDescription_Sections()
	 * @model containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List<Section> getSections();

	/**
	 * Returns the value of the '<em><b>External Id</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * An external visible number that is used to reference this delivery patterns and models. It is used like a synonym.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>External Id</em>' attribute.
	 * @see #isSetExternalId()
	 * @see #unsetExternalId()
	 * @see #setExternalId(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getContentDescription_ExternalId()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getExternalId();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.ContentDescription#getExternalId <em>External Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>External Id</em>' attribute.
	 * @see #isSetExternalId()
	 * @see #unsetExternalId()
	 * @see #getExternalId()
	 * @generated
	 */
	void setExternalId(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.ContentDescription#getExternalId <em>External Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetExternalId()
	 * @see #getExternalId()
	 * @see #setExternalId(String)
	 * @generated
	 */
	void unsetExternalId();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.ContentDescription#getExternalId <em>External Id</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>External Id</em>' attribute is set.
	 * @see #unsetExternalId()
	 * @see #getExternalId()
	 * @see #setExternalId(String)
	 * @generated
	 */
	boolean isSetExternalId();

	/**
	 * Returns the value of the '<em><b>Key Considerations</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Key Considerations provides advise and guidance of a critical nature for the content element as well as warnings, cautions, pitfalls, dangers.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Key Considerations</em>' attribute.
	 * @see #isSetKeyConsiderations()
	 * @see #unsetKeyConsiderations()
	 * @see #setKeyConsiderations(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getContentDescription_KeyConsiderations()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getKeyConsiderations();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.ContentDescription#getKeyConsiderations <em>Key Considerations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Key Considerations</em>' attribute.
	 * @see #isSetKeyConsiderations()
	 * @see #unsetKeyConsiderations()
	 * @see #getKeyConsiderations()
	 * @generated
	 */
	void setKeyConsiderations(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.ContentDescription#getKeyConsiderations <em>Key Considerations</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetKeyConsiderations()
	 * @see #getKeyConsiderations()
	 * @see #setKeyConsiderations(String)
	 * @generated
	 */
	void unsetKeyConsiderations();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.ContentDescription#getKeyConsiderations <em>Key Considerations</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Key Considerations</em>' attribute is set.
	 * @see #unsetKeyConsiderations()
	 * @see #getKeyConsiderations()
	 * @see #setKeyConsiderations(String)
	 * @generated
	 */
	boolean isSetKeyConsiderations();

} // ContentDescription
