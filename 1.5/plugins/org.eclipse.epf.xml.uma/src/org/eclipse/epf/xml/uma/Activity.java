/**
 * <copyright>
 * </copyright>
 *
 * $Id: Activity.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.util.FeatureMap;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Activity</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A Work Breakdown Element and Work Definition which supports the nesting and logical grouping of related Breakdown Elements forming breakdown structures.  Although Activity is a concrete meta-class, other classes which represent breakdown structures derive from it; such as Phase, Iteration, Delivery Process, or Capability Pattern.
 * Activity represents a grouping element for other Breakdown Elements such as Activities, Descriptors, Milestones, etc.  It is not per-se a 'high-level' grouping of only work as in other meta-models, but groups any kind of Breakdown Elements.  For example, one can define valid Activities that group only Work Products Descriptors without any matching Task Descriptors.  Activities also inherit all properties from Work Breakdown Element and indirectly from Process Element; i.e. Activity is ready to have a full content description attached to it.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getPrecondition <em>Precondition</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getPostcondition <em>Postcondition</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getGroup2 <em>Group2</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getBreakdownElement <em>Breakdown Element</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getChecklist <em>Checklist</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getConcept <em>Concept</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getExample <em>Example</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getGuideline <em>Guideline</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getRoadmap <em>Roadmap</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getReusableAsset <em>Reusable Asset</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getSupportingMaterial <em>Supporting Material</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getWhitepaper <em>Whitepaper</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#isIsEnactable <em>Is Enactable</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getVariabilityBasedOnElement <em>Variability Based On Element</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.Activity#getVariabilityType <em>Variability Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity()
 * @model extendedMetaData="name='Activity' kind='elementOnly'"
 * @generated
 */
public interface Activity extends WorkBreakdownElement {
	/**
	 * Returns the value of the '<em><b>Precondition</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Precondition</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Precondition</em>' attribute.
	 * @see #setPrecondition(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Precondition()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='Precondition'"
	 * @generated
	 */
	String getPrecondition();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.Activity#getPrecondition <em>Precondition</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Precondition</em>' attribute.
	 * @see #getPrecondition()
	 * @generated
	 */
	void setPrecondition(String value);

	/**
	 * Returns the value of the '<em><b>Postcondition</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Postcondition</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Postcondition</em>' attribute.
	 * @see #setPostcondition(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Postcondition()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='Postcondition'"
	 * @generated
	 */
	String getPostcondition();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.Activity#getPostcondition <em>Postcondition</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Postcondition</em>' attribute.
	 * @see #getPostcondition()
	 * @generated
	 */
	void setPostcondition(String value);

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
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Group2()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.EFeatureMapEntry" many="true"
	 *        extendedMetaData="kind='group' name='group:27'"
	 * @generated
	 */
	FeatureMap getGroup2();

	/**
	 * Returns the value of the '<em><b>Breakdown Element</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.xml.uma.BreakdownElement}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Breakdown Element</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Breakdown Element</em>' containment reference list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_BreakdownElement()
	 * @model type="org.eclipse.epf.xml.uma.BreakdownElement" containment="true" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='BreakdownElement' group='#group:27'"
	 * @generated
	 */
	EList getBreakdownElement();

	/**
	 * Returns the value of the '<em><b>Checklist</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Checklist</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Checklist</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Checklist()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Checklist' group='#group:27'"
	 * @generated
	 */
	EList getChecklist();

	/**
	 * Returns the value of the '<em><b>Concept</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Concept</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Concept</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Concept()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Concept' group='#group:27'"
	 * @generated
	 */
	EList getConcept();

	/**
	 * Returns the value of the '<em><b>Example</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Example</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Example</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Example()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Example' group='#group:27'"
	 * @generated
	 */
	EList getExample();

	/**
	 * Returns the value of the '<em><b>Guideline</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Guideline</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Guideline</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Guideline()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Guideline' group='#group:27'"
	 * @generated
	 */
	EList getGuideline();

	/**
	 * Returns the value of the '<em><b>Roadmap</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Roadmap</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Roadmap</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Roadmap()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Roadmap' group='#group:27'"
	 * @generated
	 */
	EList getRoadmap();

	/**
	 * Returns the value of the '<em><b>Reusable Asset</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reusable Asset</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reusable Asset</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_ReusableAsset()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='ReusableAsset' group='#group:27'"
	 * @generated
	 */
	EList getReusableAsset();

	/**
	 * Returns the value of the '<em><b>Supporting Material</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Supporting Material</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Supporting Material</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_SupportingMaterial()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='SupportingMaterial' group='#group:27'"
	 * @generated
	 */
	EList getSupportingMaterial();

	/**
	 * Returns the value of the '<em><b>Whitepaper</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Whitepaper</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Whitepaper</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_Whitepaper()
	 * @model type="java.lang.String" unique="false" dataType="org.eclipse.emf.ecore.xml.type.String" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='Whitepaper' group='#group:27'"
	 * @generated
	 */
	EList getWhitepaper();

	/**
	 * Returns the value of the '<em><b>Is Enactable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Is Enactable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Enactable</em>' attribute.
	 * @see #isSetIsEnactable()
	 * @see #unsetIsEnactable()
	 * @see #setIsEnactable(boolean)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_IsEnactable()
	 * @model unique="false" unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Boolean"
	 *        extendedMetaData="kind='attribute' name='IsEnactable'"
	 * @generated
	 */
	boolean isIsEnactable();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.Activity#isIsEnactable <em>Is Enactable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Enactable</em>' attribute.
	 * @see #isSetIsEnactable()
	 * @see #unsetIsEnactable()
	 * @see #isIsEnactable()
	 * @generated
	 */
	void setIsEnactable(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.xml.uma.Activity#isIsEnactable <em>Is Enactable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetIsEnactable()
	 * @see #isIsEnactable()
	 * @see #setIsEnactable(boolean)
	 * @generated
	 */
	void unsetIsEnactable();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.xml.uma.Activity#isIsEnactable <em>Is Enactable</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Is Enactable</em>' attribute is set.
	 * @see #unsetIsEnactable()
	 * @see #isIsEnactable()
	 * @see #setIsEnactable(boolean)
	 * @generated
	 */
	boolean isSetIsEnactable();

	/**
	 * Returns the value of the '<em><b>Variability Based On Element</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Variability Based On Element</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Variability Based On Element</em>' attribute.
	 * @see #setVariabilityBasedOnElement(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_VariabilityBasedOnElement()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='attribute' name='variabilityBasedOnElement'"
	 * @generated
	 */
	String getVariabilityBasedOnElement();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.Activity#getVariabilityBasedOnElement <em>Variability Based On Element</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Variability Based On Element</em>' attribute.
	 * @see #getVariabilityBasedOnElement()
	 * @generated
	 */
	void setVariabilityBasedOnElement(String value);

	/**
	 * Returns the value of the '<em><b>Variability Type</b></em>' attribute.
	 * The default value is <code>"na"</code>.
	 * The literals are from the enumeration {@link org.eclipse.epf.xml.uma.VariabilityType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Variability Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Variability Type</em>' attribute.
	 * @see org.eclipse.epf.xml.uma.VariabilityType
	 * @see #isSetVariabilityType()
	 * @see #unsetVariabilityType()
	 * @see #setVariabilityType(VariabilityType)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getActivity_VariabilityType()
	 * @model default="na" unique="false" unsettable="true"
	 *        extendedMetaData="kind='attribute' name='variabilityType'"
	 * @generated
	 */
	VariabilityType getVariabilityType();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.Activity#getVariabilityType <em>Variability Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Variability Type</em>' attribute.
	 * @see org.eclipse.epf.xml.uma.VariabilityType
	 * @see #isSetVariabilityType()
	 * @see #unsetVariabilityType()
	 * @see #getVariabilityType()
	 * @generated
	 */
	void setVariabilityType(VariabilityType value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.xml.uma.Activity#getVariabilityType <em>Variability Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetVariabilityType()
	 * @see #getVariabilityType()
	 * @see #setVariabilityType(VariabilityType)
	 * @generated
	 */
	void unsetVariabilityType();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.xml.uma.Activity#getVariabilityType <em>Variability Type</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Variability Type</em>' attribute is set.
	 * @see #unsetVariabilityType()
	 * @see #getVariabilityType()
	 * @see #setVariabilityType(VariabilityType)
	 * @generated
	 */
	boolean isSetVariabilityType();

} // Activity