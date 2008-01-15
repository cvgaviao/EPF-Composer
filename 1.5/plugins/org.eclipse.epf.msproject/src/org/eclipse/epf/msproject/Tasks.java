/**
 * <copyright>
 * </copyright>
 *
 * $Id: Tasks.java,v 1.1 2008/01/15 08:52:46 jtham Exp $
 */
package org.eclipse.epf.msproject;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Tasks</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.Tasks#getTask <em>Task</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.msproject.MsprojectPackage#getTasks()
 * @model extendedMetaData="name='Tasks_._type' kind='elementOnly'"
 * @generated
 */
public interface Tasks extends EObject {
	/**
	 * Returns the value of the '<em><b>Task</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.msproject.Task}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * There must be at least one task in each Tasks collection.
	 *                     
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Task</em>' containment reference list.
	 * @see org.eclipse.epf.msproject.MsprojectPackage#getTasks_Task()
	 * @model type="org.eclipse.epf.msproject.Task" containment="true" resolveProxies="false"
	 *        extendedMetaData="kind='element' name='Task' namespace='##targetNamespace'"
	 * @generated
	 */
	EList getTask();

} // Tasks
