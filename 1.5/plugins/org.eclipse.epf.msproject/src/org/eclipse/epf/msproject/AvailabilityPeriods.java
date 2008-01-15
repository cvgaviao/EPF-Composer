/**
 * <copyright>
 * </copyright>
 *
 * $Id: AvailabilityPeriods.java,v 1.1 2008/01/15 08:52:45 jtham Exp $
 */
package org.eclipse.epf.msproject;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Availability Periods</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.AvailabilityPeriods#getAvailabilityPeriod <em>Availability Period</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.msproject.MsprojectPackage#getAvailabilityPeriods()
 * @model extendedMetaData="name='AvailabilityPeriods_._type' kind='elementOnly'"
 * @generated
 */
public interface AvailabilityPeriods extends EObject {
	/**
	 * Returns the value of the '<em><b>Availability Period</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.msproject.AvailabilityPeriod}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * The period the resource is available.
	 *                                             
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Availability Period</em>' containment reference list.
	 * @see org.eclipse.epf.msproject.MsprojectPackage#getAvailabilityPeriods_AvailabilityPeriod()
	 * @model type="org.eclipse.epf.msproject.AvailabilityPeriod" containment="true" resolveProxies="false"
	 *        extendedMetaData="kind='element' name='AvailabilityPeriod' namespace='##targetNamespace'"
	 * @generated
	 */
	EList getAvailabilityPeriod();

} // AvailabilityPeriods
