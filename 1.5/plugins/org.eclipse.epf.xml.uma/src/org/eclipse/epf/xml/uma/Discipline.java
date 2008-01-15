/**
 * <copyright>
 * </copyright>
 *
 * $Id: Discipline.java,v 1.1 2008/01/15 08:52:06 jtham Exp $
 */
package org.eclipse.epf.xml.uma;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.util.FeatureMap;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Discipline</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A categorization of work (i.e. Tasks for Method Content), based upon similarity of concerns and cooperation of work effort.
 * A discipline is a collection of Tasks that are related to a major 'area of concern' within the overall project. The grouping of Tasks into disciplines is mainly an aid to understanding the project from a 'traditional' waterfall perspective. However, typically, for example, it is more common to perform certain requirements activities in close coordination with analysis and design activities. Separating these activities into separate disciplines makes the activities easier to comprehend.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.Discipline#getGroup2 <em>Group2</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Discipline#getTask <em>Task</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Discipline#getSubDiscipline <em>Sub Discipline</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Discipline#getReferenceWorkflow <em>Reference Workflow</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getDiscipline()
 * @model extendedMetaData="name='Discipline' kind='elementOnly'"
 * @generated
 */
public interface Discipline extends ContentCategory {
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
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDiscipline_Group2()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.EFeatureMapEntry" many="true"
	 *        extendedMetaData="kind='group' name='group:22'"
	 * @generated
	 */
	FeatureMap getGroup2();

	/**
	 * Returns the value of the '<em><b>Task</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Task</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Task</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDiscipline_Task()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Task' group='#group:22'"
	 * @generated
	 */
	EList getTask();

	/**
	 * Returns the value of the '<em><b>Sub Discipline</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.xml.uma.Discipline}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sub Discipline</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sub Discipline</em>' containment reference list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDiscipline_SubDiscipline()
	 * @model type="org.eclipse.epf.xml.uma.Discipline" containment="true" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='SubDiscipline' group='#group:22'"
	 * @generated
	 */
	EList getSubDiscipline();

	/**
	 * Returns the value of the '<em><b>Reference Workflow</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reference Workflow</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reference Workflow</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDiscipline_ReferenceWorkflow()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='ReferenceWorkflow' group='#group:22'"
	 * @generated
	 */
	EList getReferenceWorkflow();

} // Discipline