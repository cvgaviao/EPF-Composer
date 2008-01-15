/**
 * <copyright>
 * </copyright>
 *
 * $Id: Region.java,v 1.1 2008/01/15 08:52:02 jtham Exp $
 */
package org.eclipse.epf.uma;

import java.util.List;

import org.eclipse.epf.uma.ecore.IModelObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Region</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Region#getVertex <em>Vertex</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Region#getTransition <em>Transition</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Region#getState <em>State</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Region#getStateMachine <em>State Machine</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getRegion()
 * @model
 * @extends IModelObject
 * @generated
 */
public interface Region extends IModelObject {
	/**
	 * Returns the value of the '<em><b>Vertex</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Vertex}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Vertex#getContainer_ <em>Container</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Vertex</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Vertex</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getRegion_Vertex()
	 * @see org.eclipse.epf.uma.Vertex#getContainer_
	 * @model type="org.eclipse.epf.uma.Vertex" opposite="container" containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List getVertex();

	/**
	 * Returns the value of the '<em><b>Transition</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Transition}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Transition#getContainer_ <em>Container</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transition</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transition</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getRegion_Transition()
	 * @see org.eclipse.epf.uma.Transition#getContainer_
	 * @model type="org.eclipse.epf.uma.Transition" opposite="container" containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List getTransition();

	/**
	 * Returns the value of the '<em><b>State</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.State#getRegion <em>Region</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>State</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>State</em>' container reference.
	 * @see #setState(State)
	 * @see org.eclipse.epf.uma.UmaPackage#getRegion_State()
	 * @see org.eclipse.epf.uma.State#getRegion
	 * @model opposite="Region"
	 * @generated
	 */
	State getState();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Region#getState <em>State</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>State</em>' container reference.
	 * @see #getState()
	 * @generated
	 */
	void setState(State value);

	/**
	 * Returns the value of the '<em><b>State Machine</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.StateMachine#getRegion <em>Region</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>State Machine</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>State Machine</em>' container reference.
	 * @see #setStateMachine(StateMachine)
	 * @see org.eclipse.epf.uma.UmaPackage#getRegion_StateMachine()
	 * @see org.eclipse.epf.uma.StateMachine#getRegion
	 * @model opposite="Region"
	 * @generated
	 */
	StateMachine getStateMachine();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Region#getStateMachine <em>State Machine</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>State Machine</em>' container reference.
	 * @see #getStateMachine()
	 * @generated
	 */
	void setStateMachine(StateMachine value);

} // Region