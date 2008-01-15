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
 * A representation of the model object '<em><b>Activity</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * An Activity is a Work Breakdown Element and Work Definition which supports the nesting and logical grouping of related Breakdown Elements forming breakdown structures.  Although Activity is a concrete meta-class, other classes which represent breakdown structures derive from it; such as Phase, Iteration, Delivery Process, or Capability Pattern.
 * Activity represents a grouping element for other Breakdown Elements such as Activities, Descriptors, Milestones, etc.  It is not per-se a 'high-level' grouping of only work as in other meta-models, but groups any kind of Breakdown Elements.  For example, one can define valid Activities that group only Work Products Descriptors without any matching Task Descriptors.  Activities also inherit all properties from Work Breakdown Element and indirectly from Process Element; i.e. Activity is ready to have a full content description attached to it.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Activity#getBreakdownElements <em>Breakdown Elements</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getRoadmaps <em>Roadmaps</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getSupportingMaterials <em>Supporting Materials</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getChecklists <em>Checklists</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getConcepts <em>Concepts</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getExamples <em>Examples</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getGuidelines <em>Guidelines</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getReusableAssets <em>Reusable Assets</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Activity#getIsEnactable <em>Is Enactable</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getActivity()
 * @model
 * @generated
 */
public interface Activity extends WorkBreakdownElement, VariabilityElement,
		WorkDefinition {
	/**
	 * Returns the value of the '<em><b>Breakdown Elements</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.BreakdownElement}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.epf.uma.BreakdownElement#getSuperActivities <em>Super Activities</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Breakdown Elements</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Breakdown Elements</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_BreakdownElements()
	 * @see org.eclipse.epf.uma.BreakdownElement#getSuperActivities
	 * @model type="org.eclipse.epf.uma.BreakdownElement" opposite="superActivities" ordered="false"
	 * @generated
	 */
	List getBreakdownElements();

	/**
	 * Returns the value of the '<em><b>Roadmaps</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Roadmap}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Roadmaps</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Roadmaps</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_Roadmaps()
	 * @model type="org.eclipse.epf.uma.Roadmap" ordered="false"
	 * @generated
	 */
	List getRoadmaps();

	/**
	 * Returns the value of the '<em><b>Supporting Materials</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.SupportingMaterial}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Supporting Materials</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Supporting Materials</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_SupportingMaterials()
	 * @model type="org.eclipse.epf.uma.SupportingMaterial" ordered="false"
	 * @generated
	 */
	List getSupportingMaterials();

	/**
	 * Returns the value of the '<em><b>Checklists</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Checklist}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Checklists</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Checklists</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_Checklists()
	 * @model type="org.eclipse.epf.uma.Checklist" ordered="false"
	 * @generated
	 */
	List getChecklists();

	/**
	 * Returns the value of the '<em><b>Concepts</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Concept}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Concepts</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Concepts</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_Concepts()
	 * @model type="org.eclipse.epf.uma.Concept" ordered="false"
	 * @generated
	 */
	List getConcepts();

	/**
	 * Returns the value of the '<em><b>Examples</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Example}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Examples</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Examples</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_Examples()
	 * @model type="org.eclipse.epf.uma.Example" ordered="false"
	 * @generated
	 */
	List getExamples();

	/**
	 * Returns the value of the '<em><b>Guidelines</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Guideline}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Guidelines</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Guidelines</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_Guidelines()
	 * @model type="org.eclipse.epf.uma.Guideline" ordered="false"
	 * @generated
	 */
	List getGuidelines();

	/**
	 * Returns the value of the '<em><b>Reusable Assets</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.ReusableAsset}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reusable Assets</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reusable Assets</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_ReusableAssets()
	 * @model type="org.eclipse.epf.uma.ReusableAsset" ordered="false"
	 * @generated
	 */
	List getReusableAssets();

	/**
	 * Returns the value of the '<em><b>Is Enactable</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Is Enactable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Enactable</em>' attribute.
	 * @see #setIsEnactable(Boolean)
	 * @see org.eclipse.epf.uma.UmaPackage#getActivity_IsEnactable()
	 * @model default="false"
	 * @generated
	 */
	Boolean getIsEnactable();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Activity#getIsEnactable <em>Is Enactable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Enactable</em>' attribute.
	 * @see #getIsEnactable()
	 * @generated
	 */
	void setIsEnactable(Boolean value);

} // Activity
