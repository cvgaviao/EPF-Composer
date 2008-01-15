/**
 * <copyright>
 * </copyright>
 *
 * $Id: ContentPackage.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.util.FeatureMap;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Content Package</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A special Method Package that contains Content Elements and Content Elements, only.  Examples for Content Element are Artifacts, Tasks, Roles, etc.  A key separation of concerns in UMA is the distinction between Method Content and Process.  This separation is enforced by special package types, which do not allow the mixing of method content with processes.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.ContentPackage#getGroup2 <em>Group2</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.ContentPackage#getContentElement <em>Content Element</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getContentPackage()
 * @model extendedMetaData="name='ContentPackage' kind='elementOnly'"
 * @generated
 */
public interface ContentPackage extends MethodPackage {
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
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getContentPackage_Group2()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.EFeatureMapEntry" many="true"
	 *        extendedMetaData="kind='group' name='group:12'"
	 * @generated
	 */
	FeatureMap getGroup2();

	/**
	 * Returns the value of the '<em><b>Content Element</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.xml.uma.ContentElement}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Content Element</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Content Element</em>' containment reference list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getContentPackage_ContentElement()
	 * @model type="org.eclipse.epf.xml.uma.ContentElement" containment="true" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="kind='element' name='ContentElement' group='#group:12'"
	 * @generated
	 */
	EList getContentElement();

} // ContentPackage