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
 * A representation of the model object '<em><b>Task Descriptor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A Task Descriptor is a Descriptor and Work Breakdown Element that represents a proxy for a Task in the context of one specific Activity.  Every breakdown structure can define different relationships of Task Descriptors to Work Product Descriptors and Role Descriptors. Therefore one Task can be represented by many Task Descriptors each within the context of an Activity with its own set of relationships.
 * A key difference between Method Content and Process is that a Content Element such as Task describes all aspects of doing work defined around this Task.  This description is managed in steps, which are modeled as Sections of the Tasks' Content Descriptions.  When applying a Task in a Process' Activity with a Task Descriptor a Process Engineer needs to indicate that at that particular point in time in the Process definition for which the Task Descriptor has been created, only a subset of steps shall be performed.  He defines this selection using the selectedSteps association.  If he wants to add steps to a Task Descriptor, he can describe these either pragmatically in the refinedDescription attribute or 'properly' create a contributing Task to the Task the Task Descriptor refers to.
 * 
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getTask <em>Task</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getAdditionallyPerformedBy <em>Additionally Performed By</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getAssistedBy <em>Assisted By</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getExternalInput <em>External Input</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getMandatoryInput <em>Mandatory Input</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getOptionalInput <em>Optional Input</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getOutput <em>Output</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getPerformedPrimarilyBy <em>Performed Primarily By</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.TaskDescriptor#getSelectedSteps <em>Selected Steps</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor()
 * @model
 * @generated
 */
public interface TaskDescriptor extends WorkBreakdownElement, Descriptor {
	/**
	 * Returns the value of the '<em><b>Task</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Task</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Task</em>' reference.
	 * @see #setTask(Task)
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_Task()
	 * @model
	 * @generated
	 */
	Task getTask();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.TaskDescriptor#getTask <em>Task</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Task</em>' reference.
	 * @see #getTask()
	 * @generated
	 */
	void setTask(Task value);

	/**
	 * Returns the value of the '<em><b>Additionally Performed By</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.RoleDescriptor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Additionally Performed By</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Additionally Performed By</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_AdditionallyPerformedBy()
	 * @model type="org.eclipse.epf.uma.RoleDescriptor" ordered="false"
	 * @generated
	 */
	List getAdditionallyPerformedBy();

	/**
	 * Returns the value of the '<em><b>Assisted By</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.RoleDescriptor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Assisted By</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Assisted By</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_AssistedBy()
	 * @model type="org.eclipse.epf.uma.RoleDescriptor" ordered="false"
	 * @generated
	 */
	List getAssistedBy();

	/**
	 * Returns the value of the '<em><b>External Input</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.WorkProductDescriptor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>External Input</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>External Input</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_ExternalInput()
	 * @model type="org.eclipse.epf.uma.WorkProductDescriptor" ordered="false"
	 * @generated
	 */
	List getExternalInput();

	/**
	 * Returns the value of the '<em><b>Mandatory Input</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.WorkProductDescriptor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Mandatory Input</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Mandatory Input</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_MandatoryInput()
	 * @model type="org.eclipse.epf.uma.WorkProductDescriptor" ordered="false"
	 * @generated
	 */
	List getMandatoryInput();

	/**
	 * Returns the value of the '<em><b>Optional Input</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.WorkProductDescriptor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Optional Input</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Optional Input</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_OptionalInput()
	 * @model type="org.eclipse.epf.uma.WorkProductDescriptor" ordered="false"
	 * @generated
	 */
	List getOptionalInput();

	/**
	 * Returns the value of the '<em><b>Output</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.WorkProductDescriptor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Output</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Output</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_Output()
	 * @model type="org.eclipse.epf.uma.WorkProductDescriptor" ordered="false"
	 * @generated
	 */
	List getOutput();

	/**
	 * Returns the value of the '<em><b>Performed Primarily By</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Performed Primarily By</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Performed Primarily By</em>' reference.
	 * @see #setPerformedPrimarilyBy(RoleDescriptor)
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_PerformedPrimarilyBy()
	 * @model
	 * @generated
	 */
	RoleDescriptor getPerformedPrimarilyBy();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.TaskDescriptor#getPerformedPrimarilyBy <em>Performed Primarily By</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Performed Primarily By</em>' reference.
	 * @see #getPerformedPrimarilyBy()
	 * @generated
	 */
	void setPerformedPrimarilyBy(RoleDescriptor value);

	/**
	 * Returns the value of the '<em><b>Selected Steps</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Section}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selected Steps</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selected Steps</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getTaskDescriptor_SelectedSteps()
	 * @model type="org.eclipse.epf.uma.Section" ordered="false"
	 * @generated
	 */
	List getSelectedSteps();

} // TaskDescriptor
