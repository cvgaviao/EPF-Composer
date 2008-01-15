/**
 * <copyright>
 * </copyright>
 *
 * $Id: Assignments.java,v 1.1 2008/01/15 08:52:45 jtham Exp $
 */
package org.eclipse.epf.msproject;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Assignments</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.Assignments#getAssignment <em>Assignment</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.msproject.MsprojectPackage#getAssignments()
 * @model extendedMetaData="name='Assignments_._type' kind='elementOnly'"
 * @generated
 */
public interface Assignments extends EObject {
	/**
	 * Returns the value of the '<em><b>Assignment</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.msproject.Assignment}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * There must be at least one assignment in each Assignments collection.
	 *                 
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Assignment</em>' containment reference list.
	 * @see org.eclipse.epf.msproject.MsprojectPackage#getAssignments_Assignment()
	 * @model type="org.eclipse.epf.msproject.Assignment" containment="true" resolveProxies="false"
	 *        extendedMetaData="kind='element' name='Assignment' namespace='##targetNamespace'"
	 * @generated
	 */
	EList getAssignment();

} // Assignments
