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

import org.eclipse.epf.uma.ecore.IModelObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Vertex</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Vertex#getContainer_ <em>Container</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Vertex#getOutgoing <em>Outgoing</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Vertex#getIncoming <em>Incoming</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getVertex()
 * @model
 * @extends IModelObject
 * @generated
 */
public interface Vertex extends IModelObject {
	/**
	 * Returns the value of the '<em><b>Container</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Region#getVertex <em>Vertex</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Container</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Container</em>' container reference.
	 * @see #setContainer(Region)
	 * @see org.eclipse.epf.uma.UmaPackage#getVertex_Container()
	 * @see org.eclipse.epf.uma.Region#getVertex
	 * @model opposite="Vertex"
	 * @generated
	 */
	Region getContainer_();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Vertex#getContainer_ <em>Container</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Container</em>' container reference.
	 * @see #getContainer_()
	 * @generated
	 */
	void setContainer(Region value);

	/**
	 * Returns the value of the '<em><b>Outgoing</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Transition}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Transition#getSource <em>Source</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Outgoing</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Outgoing</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getVertex_Outgoing()
	 * @see org.eclipse.epf.uma.Transition#getSource
	 * @model type="org.eclipse.epf.uma.Transition" opposite="source" ordered="false"
	 * @generated
	 */
	List getOutgoing();

	/**
	 * Returns the value of the '<em><b>Incoming</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Transition}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Transition#getTarget <em>Target</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Incoming</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Incoming</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getVertex_Incoming()
	 * @see org.eclipse.epf.uma.Transition#getTarget
	 * @model type="org.eclipse.epf.uma.Transition" opposite="target" ordered="false"
	 * @generated
	 */
	List getIncoming();

} // Vertex