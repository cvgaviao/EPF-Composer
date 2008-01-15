/**
 * <copyright>
 * </copyright>
 *
 * $Id: State.java,v 1.1 2008/01/15 08:52:02 jtham Exp $
 */
package org.eclipse.epf.uma;

import java.util.List;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>State</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.State#getWorkProduct <em>Work Product</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.State#getRegion <em>Region</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.State#getSubmachine <em>Submachine</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getState()
 * @model
 * @generated
 */
public interface State extends Vertex {
	/**
	 * Returns the value of the '<em><b>Work Product</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.WorkProduct}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Work Product</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Work Product</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getState_WorkProduct()
	 * @model type="org.eclipse.epf.uma.WorkProduct" required="true" ordered="false"
	 * @generated
	 */
	List getWorkProduct();

	/**
	 * Returns the value of the '<em><b>Region</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Region}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Region#getState <em>State</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Region</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Region</em>' containment reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getState_Region()
	 * @see org.eclipse.epf.uma.Region#getState
	 * @model type="org.eclipse.epf.uma.Region" opposite="State" containment="true" resolveProxies="true" ordered="false"
	 * @generated
	 */
	List getRegion();

	/**
	 * Returns the value of the '<em><b>Submachine</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Submachine</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Submachine</em>' reference.
	 * @see #setSubmachine(StateMachine)
	 * @see org.eclipse.epf.uma.UmaPackage#getState_Submachine()
	 * @model
	 * @generated
	 */
	StateMachine getSubmachine();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.State#getSubmachine <em>Submachine</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Submachine</em>' reference.
	 * @see #getSubmachine()
	 * @generated
	 */
	void setSubmachine(StateMachine value);

} // State