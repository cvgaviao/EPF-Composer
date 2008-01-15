/**
 * <copyright>
 * </copyright>
 *
 * $Id: ValueList.java,v 1.1 2008/01/15 08:52:46 jtham Exp $
 */
package org.eclipse.epf.msproject;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Value List</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.ValueList#getValue <em>Value</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.msproject.MsprojectPackage#getValueList()
 * @model extendedMetaData="name='ValueList_._type' kind='elementOnly'"
 * @generated
 */
public interface ValueList extends EObject {
	/**
	 * Returns the value of the '<em><b>Value</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.msproject.Value2}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * The values that make up the value list.
	 *                                             
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Value</em>' containment reference list.
	 * @see org.eclipse.epf.msproject.MsprojectPackage#getValueList_Value()
	 * @model type="org.eclipse.epf.msproject.Value2" containment="true" resolveProxies="false" required="true"
	 *        extendedMetaData="kind='element' name='Value' namespace='##targetNamespace'"
	 * @generated
	 */
	EList getValue();

} // ValueList
