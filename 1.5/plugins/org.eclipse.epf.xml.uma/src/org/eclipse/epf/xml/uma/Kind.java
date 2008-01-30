/**
 * <copyright>
 * </copyright>
 *
 * $Id: Kind.java,v 1.1 2008/01/30 00:41:48 klow Exp $
 */
package org.eclipse.epf.xml.uma;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Kind</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.Kind#getApplicableMetaClassInfo <em>Applicable Meta Class Info</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getKind()
 * @model extendedMetaData="name='Kind' kind='elementOnly'"
 * @generated
 */
public interface Kind extends ContentElement {
	/**
	 * Returns the value of the '<em><b>Applicable Meta Class Info</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Applicable Meta Class Info</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Applicable Meta Class Info</em>' attribute list.
	 * @see org.eclipse.epf.xml.uma.UmaPackage#getKind_ApplicableMetaClassInfo()
	 * @model unique="false" dataType="org.eclipse.emf.ecore.xml.type.String"
	 *        extendedMetaData="kind='element' name='ApplicableMetaClassInfo'"
	 * @generated
	 */
	EList<String> getApplicableMetaClassInfo();

} // Kind
