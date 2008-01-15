/**
 * <copyright>
 * </copyright>
 *
 * $Id: Transition.java,v 1.1 2008/01/15 08:52:02 jtham Exp $
 */
package org.eclipse.epf.uma;

import java.util.List;

import org.eclipse.epf.uma.ecore.IModelObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Transition</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Transition#getWorkDefinition <em>Work Definition</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Transition#getContainer_ <em>Container</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Transition#getSource <em>Source</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Transition#getTarget <em>Target</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getTransition()
 * @model
 * @extends IModelObject
 * @generated
 */
public interface Transition extends IModelObject {
	/**
	 * Returns the value of the '<em><b>Work Definition</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.WorkDefinition}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Work Definition</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Work Definition</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTransition_WorkDefinition()
	 * @model type="org.eclipse.epf.uma.WorkDefinition" required="true" ordered="false"
	 * @generated
	 */
	List getWorkDefinition();

	/**
	 * Returns the value of the '<em><b>Container</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Region#getTransition <em>Transition</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Container</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Container</em>' container reference.
	 * @see #setContainer(Region)
	 * @see org.eclipse.epf.uma.UmaPackage#getTransition_Container()
	 * @see org.eclipse.epf.uma.Region#getTransition
	 * @model opposite="Transition" required="true"
	 * @generated
	 */
	Region getContainer_();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Transition#getContainer_ <em>Container</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Container</em>' container reference.
	 * @see #getContainer_()
	 * @generated
	 */
	void setContainer(Region value);

	/**
	 * Returns the value of the '<em><b>Source</b></em>' reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Vertex#getOutgoing <em>Outgoing</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Source</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Source</em>' reference.
	 * @see #setSource(Vertex)
	 * @see org.eclipse.epf.uma.UmaPackage#getTransition_Source()
	 * @see org.eclipse.epf.uma.Vertex#getOutgoing
	 * @model opposite="outgoing" required="true"
	 * @generated
	 */
	Vertex getSource();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Transition#getSource <em>Source</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Source</em>' reference.
	 * @see #getSource()
	 * @generated
	 */
	void setSource(Vertex value);

	/**
	 * Returns the value of the '<em><b>Target</b></em>' reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.Vertex#getIncoming <em>Incoming</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Target</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Target</em>' reference.
	 * @see #setTarget(Vertex)
	 * @see org.eclipse.epf.uma.UmaPackage#getTransition_Target()
	 * @see org.eclipse.epf.uma.Vertex#getIncoming
	 * @model opposite="incoming" required="true"
	 * @generated
	 */
	Vertex getTarget();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Transition#getTarget <em>Target</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Target</em>' reference.
	 * @see #getTarget()
	 * @generated
	 */
	void setTarget(Vertex value);

} // Transition