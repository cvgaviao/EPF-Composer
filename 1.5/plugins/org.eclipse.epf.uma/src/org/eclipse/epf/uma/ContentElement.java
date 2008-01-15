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
 * A representation of the model object '<em><b>Content Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * Content Element is a Describable Element that represents an abstract generalization for all elements that are considered to be and managed as Method Content.
 * Content Elements represents reusable Method Content that is supposed to be managed in Content Packages.  The separation of Content Element from Process Element allows to clearly distinguish between pure method content from content that is represented in processes.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getSupportingMaterials <em>Supporting Materials</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getConceptsAndPapers <em>Concepts And Papers</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getChecklists <em>Checklists</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getGuidelines <em>Guidelines</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getExamples <em>Examples</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getAssets <em>Assets</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ContentElement#getTermDefinition <em>Term Definition</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getContentElement()
 * @model abstract="true"
 * @generated
 */
public interface ContentElement extends DescribableElement, VariabilityElement {
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
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_SupportingMaterials()
	 * @model type="org.eclipse.epf.uma.SupportingMaterial" ordered="false"
	 * @generated
	 */
	List getSupportingMaterials();

	/**
	 * Returns the value of the '<em><b>Concepts And Papers</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.Concept}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Concepts And Papers</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Concepts And Papers</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_ConceptsAndPapers()
	 * @model type="org.eclipse.epf.uma.Concept" ordered="false"
	 * @generated
	 */
	List getConceptsAndPapers();

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
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_Checklists()
	 * @model type="org.eclipse.epf.uma.Checklist" ordered="false"
	 * @generated
	 */
	List getChecklists();

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
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_Guidelines()
	 * @model type="org.eclipse.epf.uma.Guideline" ordered="false"
	 * @generated
	 */
	List getGuidelines();

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
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_Examples()
	 * @model type="org.eclipse.epf.uma.Example" ordered="false"
	 * @generated
	 */
	List getExamples();

	/**
	 * Returns the value of the '<em><b>Assets</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.ReusableAsset}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Assets</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Assets</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_Assets()
	 * @model type="org.eclipse.epf.uma.ReusableAsset" ordered="false"
	 * @generated
	 */
	List getAssets();

	/**
	 * Returns the value of the '<em><b>Term Definition</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.epf.uma.TermDefinition}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Term Definition</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Term Definition</em>' reference list.
	 * @see org.eclipse.epf.uma.UmaPackage#getContentElement_TermDefinition()
	 * @model type="org.eclipse.epf.uma.TermDefinition" ordered="false"
	 * @generated
	 */
	List getTermDefinition();

} // ContentElement
