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
 * A representation of the model object '<em><b>Graph Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.GraphElement#getPosition <em>Position</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.GraphElement#getContained <em>Contained</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.GraphElement#getLink <em>Link</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.GraphElement#getAnchorage <em>Anchorage</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.GraphElement#getSemanticModel <em>Semantic Model</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getGraphElement()
 * @model abstract="true"
 * @generated
 */
public interface GraphElement extends DiagramElement {
	/**
	 * Returns the value of the '<em><b>Position</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Position</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Position</em>' containment reference.
	 * @see #setPosition(Point)
	 * @see org.eclipse.epf.uma.UmaPackage#getGraphElement_Position()
	 * @model containment="true" resolveProxies="true"
	 * @generated
	 */
	Point getPosition();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.GraphElement#getPosition <em>Position</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Position</em>' containment reference.
	 * @see #getPosition()
	 * @generated
	 */
	void setPosition(Point value);

	/**
	 * Returns the value of the '<em><b>Contained</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.DiagramElement}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.DiagramElement#getContainer_ <em>Container</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contained</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contained</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getGraphElement_Contained()
	 * @see org.eclipse.epf.uma.DiagramElement#getContainer_
	 * @model type="org.eclipse.epf.uma.DiagramElement" opposite="container" containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List getContained();

	/**
	 * Returns the value of the '<em><b>Link</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.DiagramLink}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.DiagramLink#getGraphElement <em>Graph Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Link</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Link</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getGraphElement_Link()
	 * @see org.eclipse.epf.uma.DiagramLink#getGraphElement
	 * @model type="org.eclipse.epf.uma.DiagramLink" opposite="graphElement" containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List getLink();

	/**
	 * Returns the value of the '<em><b>Anchorage</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.GraphConnector}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.GraphConnector#getGraphElement <em>Graph Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Anchorage</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Anchorage</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getGraphElement_Anchorage()
	 * @see org.eclipse.epf.uma.GraphConnector#getGraphElement
	 * @model type="org.eclipse.epf.uma.GraphConnector" opposite="graphElement" containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List getAnchorage();

	/**
	 * Returns the value of the '<em><b>Semantic Model</b></em>' containment reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.SemanticModelBridge#getGraphElement <em>Graph Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Semantic Model</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Semantic Model</em>' containment reference.
	 * @see #setSemanticModel(SemanticModelBridge)
	 * @see org.eclipse.epf.uma.UmaPackage#getGraphElement_SemanticModel()
	 * @see org.eclipse.epf.uma.SemanticModelBridge#getGraphElement
	 * @model opposite="graphElement" containment="true" resolveProxies="true"
	 * @generated
	 */
	SemanticModelBridge getSemanticModel();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.GraphElement#getSemanticModel <em>Semantic Model</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Semantic Model</em>' containment reference.
	 * @see #getSemanticModel()
	 * @generated
	 */
	void setSemanticModel(SemanticModelBridge value);

} // GraphElement
