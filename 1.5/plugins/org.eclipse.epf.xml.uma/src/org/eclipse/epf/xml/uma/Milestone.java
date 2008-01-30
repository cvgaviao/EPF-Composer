/**
 * <copyright>
 * </copyright>
 *
 * $Id: Milestone.java,v 1.2 2008/01/30 00:41:47 klow Exp $
 */
package org.eclipse.epf.xml.uma;

import org.eclipse.emf.common.util.EList;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Milestone</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A special Breakdown Element that describes a significant event in a development project, such as a major decision, completion of a deliverable, or meeting of a major dependency (like completion of a project phase).  Because, Milestone is commonly used to refer to both the event itself and the point in time at which the event is scheduled to happen, it is modeled as a Breakdown Element (i.e. it appears as part of a breakdown structure).
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.Milestone#getRequiredResult <em>Required Result</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getMilestone()
 * @model extendedMetaData="name='Milestone' kind='elementOnly'"
 * @generated
 */
public interface Milestone extends WorkBreakdownElement {

	/**
	 * Returns the value of the '<em><b>Required Result</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Required Result</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Required Result</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getMilestone_RequiredResult()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='RequiredResult'"
	 * @generated
	 */
	EList<String> getRequiredResult();
} // Milestone