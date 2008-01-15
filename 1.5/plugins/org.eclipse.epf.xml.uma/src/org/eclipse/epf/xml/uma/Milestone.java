/**
 * <copyright>
 * </copyright>
 *
 * $Id: Milestone.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Milestone</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A special Breakdown Element that describes a significant event in a development project, such as a major decision, completion of a deliverable, or meeting of a major dependency (like completion of a project phase).  Because, Milestone is commonly used to refer to both the event itself and the point in time at which the event is scheduled to happen, it is modeled as a Breakdown Element (i.e. it appears as part of a breakdown structure).
 * <!-- end-model-doc -->
 *
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getMilestone()
 * @model extendedMetaData="name='Milestone' kind='elementOnly'"
 * @generated
 */
public interface Milestone extends WorkBreakdownElement {
} // Milestone