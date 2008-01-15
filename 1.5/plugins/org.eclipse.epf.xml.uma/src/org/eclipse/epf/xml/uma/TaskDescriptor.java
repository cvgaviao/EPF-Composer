/**
 * <copyright>
 * </copyright>
 *
 * $Id: TaskDescriptor.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.util.FeatureMap;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Task Descriptor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A special Descriptor that represents a proxy for a Task in the context of one specific Activity.  Every breakdown structure can define different relationships of Task Descriptors to Work Product Descriptors and Role Descriptors. Therefore one Task can be represented by many Task Descriptors each within the context of an Activity with its own set of relationships.
 * A key difference between Method Content and Process is that a Content Element such as Task describes all aspects of doing work defined around this Task.  This description is managed in steps, which are modeled as Sections of the Tasks' Content Descriptions.  When applying a Task in a Process' Activity with a Task Descriptor a Process Engineer needs to indicate that at that particular point in time in the Process definition for which the Task Descriptor has been created, only a subset of steps shall be performed.  He defines this selection using the selectedSteps association.  If he wants to add steps to a Task Descriptor, he can describe these either pragmatically in the refinedDescription attribute or 'properly' create a contributing Task to the Task the Task Descriptor refers to.
 * 
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getTask <em>Task</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getPerformedPrimarilyBy <em>Performed Primarily By</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getGroup2 <em>Group2</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getAdditionallyPerformedBy <em>Additionally Performed By</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getAssistedBy <em>Assisted By</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getExternalInput <em>External Input</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getMandatoryInput <em>Mandatory Input</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getOptionalInput <em>Optional Input</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getOutput <em>Output</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#getStep <em>Step</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.TaskDescriptor#isIsSynchronizedWithSource <em>Is Synchronized With Source</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor()
 * @model extendedMetaData="name='TaskDescriptor' kind='elementOnly'"
 * @generated
 */
public interface TaskDescriptor extends WorkBreakdownElement {
	/**
	 * Returns the value of the '<em><b>Task</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Task</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Task</em>' attribute.
	 * @see #setTask(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_Task()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='Task'"
	 * @generated
	 */
	String getTask();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.TaskDescriptor#getTask <em>Task</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Task</em>' attribute.
	 * @see #getTask()
	 * @generated
	 */
	void setTask(String value);

	/**
	 * Returns the value of the '<em><b>Performed Primarily By</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Performed Primarily By</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Performed Primarily By</em>' attribute.
	 * @see #setPerformedPrimarilyBy(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_PerformedPrimarilyBy()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='PerformedPrimarilyBy'"
	 * @generated
	 */
	String getPerformedPrimarilyBy();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.TaskDescriptor#getPerformedPrimarilyBy <em>Performed Primarily By</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Performed Primarily By</em>' attribute.
	 * @see #getPerformedPrimarilyBy()
	 * @generated
	 */
	void setPerformedPrimarilyBy(String value);

	/**
	 * Returns the value of the '<em><b>Group2</b></em>' attribute list.
	 * The list contents are of type {@link org.eclipse.emf.ecore.util.FeatureMap.Entry}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Group2</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Group2</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_Group2()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.EFeatureMapEntry" many="true"
	 *        extendedMetaData="kind='group' name='group:27'"
	 * @generated
	 */
	FeatureMap getGroup2();

	/**
	 * Returns the value of the '<em><b>Additionally Performed By</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Additionally Performed By</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Additionally Performed By</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_AdditionallyPerformedBy()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='AdditionallyPerformedBy' group='#group:27'"
	 * @generated
	 */
	EList getAdditionallyPerformedBy();

	/**
	 * Returns the value of the '<em><b>Assisted By</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Assisted By</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Assisted By</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_AssistedBy()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='AssistedBy' group='#group:27'"
	 * @generated
	 */
	EList getAssistedBy();

	/**
	 * Returns the value of the '<em><b>External Input</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>External Input</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>External Input</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_ExternalInput()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='ExternalInput' group='#group:27'"
	 * @generated
	 */
	EList getExternalInput();

	/**
	 * Returns the value of the '<em><b>Mandatory Input</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Mandatory Input</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Mandatory Input</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_MandatoryInput()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='MandatoryInput' group='#group:27'"
	 * @generated
	 */
	EList getMandatoryInput();

	/**
	 * Returns the value of the '<em><b>Optional Input</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Optional Input</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Optional Input</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_OptionalInput()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='OptionalInput' group='#group:27'"
	 * @generated
	 */
	EList getOptionalInput();

	/**
	 * Returns the value of the '<em><b>Output</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Output</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Output</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_Output()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Output' group='#group:27'"
	 * @generated
	 */
	EList getOutput();

	/**
	 * Returns the value of the '<em><b>Step</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.xml.uma.Section}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Step</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Step</em>' containment reference list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_Step()
	 * @model type="org.eclipse.epf.xml.uma.Section" containment="true"
	 *        extendedMetaData="kind='element' name='Step'"
	 * @generated
	 */
	EList getStep();

	/**
	 * Returns the value of the '<em><b>Is Synchronized With Source</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Is Synchronized With Source</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Synchronized With Source</em>' attribute.
	 * @see #isSetIsSynchronizedWithSource()
	 * @see #unsetIsSynchronizedWithSource()
	 * @see #setIsSynchronizedWithSource(boolean)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getTaskDescriptor_IsSynchronizedWithSource()
	 * @model unique="false" unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Boolean"
	 *        extendedMetaData="kind='attribute' name='isSynchronizedWithSource'"
	 * @generated
	 */
	boolean isIsSynchronizedWithSource();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.TaskDescriptor#isIsSynchronizedWithSource <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Synchronized With Source</em>' attribute.
	 * @see #isSetIsSynchronizedWithSource()
	 * @see #unsetIsSynchronizedWithSource()
	 * @see #isIsSynchronizedWithSource()
	 * @generated
	 */
	void setIsSynchronizedWithSource(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.xml.uma.TaskDescriptor#isIsSynchronizedWithSource <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetIsSynchronizedWithSource()
	 * @see #isIsSynchronizedWithSource()
	 * @see #setIsSynchronizedWithSource(boolean)
	 * @generated
	 */
	void unsetIsSynchronizedWithSource();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.xml.uma.TaskDescriptor#isIsSynchronizedWithSource <em>Is Synchronized With Source</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Is Synchronized With Source</em>' attribute is set.
	 * @see #unsetIsSynchronizedWithSource()
	 * @see #isIsSynchronizedWithSource()
	 * @see #setIsSynchronizedWithSource(boolean)
	 * @generated
	 */
	boolean isSetIsSynchronizedWithSource();

} // TaskDescriptor