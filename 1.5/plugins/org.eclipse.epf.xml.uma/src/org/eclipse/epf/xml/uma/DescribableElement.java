/**
 * <copyright>
 * </copyright>
 *
 * $Id: DescribableElement.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Describable Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * An abstract generalization of Method Elements for which external presentation names as well as content descriptions have been defined, such as Roles or Work Products.  Presentation Name and Content Descriptions are typically localized using a resource allocation mechanism for its String type attributes.
 * This abstraction represents all elements in the Method Content as well as Process space for which concrete textual descriptions are defined in the form of documenting attributes grouped in a matching Content Description instance.  Describable Elements are intended to be published in method or process publications (similar to the IBM Rational Unified Process web).  Describable Element defines that the element it represents will have content 'attached' to it.  Content Description is the abstraction for the actual places in which the content is being represented.  This separation allows a distinction between core method model elements describing the structure of the model from the actual description container providing, for example, the documentation of the content element in different alternatives languages, audiences, licensing levels, etc.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.DescribableElement#getPresentation <em>Presentation</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.DescribableElement#getNodeicon <em>Nodeicon</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.DescribableElement#getPresentationName <em>Presentation Name</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.DescribableElement#getShapeicon <em>Shapeicon</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getDescribableElement()
 * @model extendedMetaData="name='DescribableElement' kind='elementOnly'"
 * @generated
 */
public interface DescribableElement extends MethodElement {
	/**
	 * Returns the value of the '<em><b>Presentation</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Presentation</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Presentation</em>' containment reference.
	 * @see #setPresentation(ContentDescription)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDescribableElement_Presentation()
	 * @model containment="true"
	 *        extendedMetaData="kind='element' name='Presentation'"
	 * @generated
	 */
	ContentDescription getPresentation();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.DescribableElement#getPresentation <em>Presentation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Presentation</em>' containment reference.
	 * @see #getPresentation()
	 * @generated
	 */
	void setPresentation(ContentDescription value);

	/**
	 * Returns the value of the '<em><b>Nodeicon</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A reference to an icon that can be used in tree browser presentations and breakdown structures.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Nodeicon</em>' attribute.
	 * @see #setNodeicon(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDescribableElement_Nodeicon()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='attribute' name='nodeicon'"
	 * @generated
	 */
	String getNodeicon();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.DescribableElement#getNodeicon <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Nodeicon</em>' attribute.
	 * @see #getNodeicon()
	 * @generated
	 */
	void setNodeicon(String value);

	/**
	 * Returns the value of the '<em><b>Presentation Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Every Describable Element has a presentation name, which is used for external presentation of the element.  For example, name (the internal representation) might be set to "rup_architecture_document" to differentiate from a "j2ee_architcture_document" whereas the external presentation would always be "Architecture Document".
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Presentation Name</em>' attribute.
	 * @see #setPresentationName(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDescribableElement_PresentationName()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='attribute' name='presentationName'"
	 * @generated
	 */
	String getPresentationName();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.DescribableElement#getPresentationName <em>Presentation Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Presentation Name</em>' attribute.
	 * @see #getPresentationName()
	 * @generated
	 */
	void setPresentationName(String value);

	/**
	 * Returns the value of the '<em><b>Shapeicon</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A reference to an icon that can be used for modeling with specific Content Element instances (as graphical stereotypes, e.g. a use case symbol for a use case artifact) as well as publication of content.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Shapeicon</em>' attribute.
	 * @see #setShapeicon(String)
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getDescribableElement_Shapeicon()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='attribute' name='shapeicon'"
	 * @generated
	 */
	String getShapeicon();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.xml.uma.DescribableElement#getShapeicon <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Shapeicon</em>' attribute.
	 * @see #getShapeicon()
	 * @generated
	 */
	void setShapeicon(String value);

} // DescribableElement