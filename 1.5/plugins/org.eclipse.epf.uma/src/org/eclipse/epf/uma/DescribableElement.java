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
 * A representation of the model object '<em><b>Describable Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * Describable Element is an abstract generalization of Method Elements for which external presentation names as well as content descriptions have been defined, such as Roles or Work Products.  Presentation Name and Content Descriptions are typically localized using a resource allocation mechanism for its String type attributes.
 * This abstraction represents all elements in the Method Content as well as Process space for which concrete textual descriptions are defined in the form of documenting attributes grouped in a matching Content Description instance (see Section 4.1.4).  Describable Elements are intended to be published in method or process publications (similar to the IBM Rational Unified Process web).  Describable Element defines that the element it represents will have content 'attached' to it.  Content Description is the abstraction for the actual places in which the content is being represented.  This separation allows a distinction between core method model elements describing the structure of the model from the actual description container providing, for example, the documentation of the content element in different alternatives languages, audiences, licensing levels, etc.
 * 
 * This definition of Content Element extends the Content Element definition via package merge with references to icons that are used for presenting Content Elements in a UMA-based modeling environment as well as when publishing Content Elements into documentation presentation (e.g. document or html pages).
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.DescribableElement#getPresentation <em>Presentation</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DescribableElement#getShapeicon <em>Shapeicon</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DescribableElement#getNodeicon <em>Nodeicon</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getDescribableElement()
 * @model abstract="true"
 * @generated
 */
public interface DescribableElement extends MethodElement, Classifier {
	/**
	 * Returns the value of the '<em><b>Presentation</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Presentation</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Presentation</em>' containment reference.
	 * @see #setPresentation(ContentDescription)
	 * @see org.eclipse.epf.uma.UmaPackage#getDescribableElement_Presentation()
	 * @model containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	ContentDescription getPresentation();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DescribableElement#getPresentation <em>Presentation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Presentation</em>' containment reference.
	 * @see #getPresentation()
	 * @generated
	 */
	void setPresentation(ContentDescription value);

	/**
	 * Returns the value of the '<em><b>Shapeicon</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A reference to an icon that can be used for modeling with specific Content Element instances (as graphical stereotypes, e.g. a use case symbol for a use case artifact) as well as publication of content.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Shapeicon</em>' attribute.
	 * @see #isSetShapeicon()
	 * @see #unsetShapeicon()
	 * @see #setShapeicon(URI)
	 * @see org.eclipse.epf.uma.UmaPackage#getDescribableElement_Shapeicon()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.Uri" ordered="false"
	 * @generated
	 */
	URI getShapeicon();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DescribableElement#getShapeicon <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Shapeicon</em>' attribute.
	 * @see #isSetShapeicon()
	 * @see #unsetShapeicon()
	 * @see #getShapeicon()
	 * @generated
	 */
	void setShapeicon(URI value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DescribableElement#getShapeicon <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetShapeicon()
	 * @see #getShapeicon()
	 * @see #setShapeicon(URI)
	 * @generated
	 */
	void unsetShapeicon();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DescribableElement#getShapeicon <em>Shapeicon</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Shapeicon</em>' attribute is set.
	 * @see #unsetShapeicon()
	 * @see #getShapeicon()
	 * @see #setShapeicon(URI)
	 * @generated
	 */
	boolean isSetShapeicon();

	/**
	 * Returns the value of the '<em><b>Nodeicon</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A reference to an icon that can be used in tree browser presentations and breakdown structures.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Nodeicon</em>' attribute.
	 * @see #isSetNodeicon()
	 * @see #unsetNodeicon()
	 * @see #setNodeicon(URI)
	 * @see org.eclipse.epf.uma.UmaPackage#getDescribableElement_Nodeicon()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.Uri" ordered="false"
	 * @generated
	 */
	URI getNodeicon();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DescribableElement#getNodeicon <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Nodeicon</em>' attribute.
	 * @see #isSetNodeicon()
	 * @see #unsetNodeicon()
	 * @see #getNodeicon()
	 * @generated
	 */
	void setNodeicon(URI value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DescribableElement#getNodeicon <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetNodeicon()
	 * @see #getNodeicon()
	 * @see #setNodeicon(URI)
	 * @generated
	 */
	void unsetNodeicon();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DescribableElement#getNodeicon <em>Nodeicon</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Nodeicon</em>' attribute is set.
	 * @see #unsetNodeicon()
	 * @see #getNodeicon()
	 * @see #setNodeicon(URI)
	 * @generated
	 */
	boolean isSetNodeicon();

} // DescribableElement
