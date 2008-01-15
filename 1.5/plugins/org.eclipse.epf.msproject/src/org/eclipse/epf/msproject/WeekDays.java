/**
 * <copyright>
 * </copyright>
 *
 * $Id: WeekDays.java,v 1.1 2008/01/15 08:52:45 jtham Exp $
 */
package org.eclipse.epf.msproject;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Week Days</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.WeekDays#getWeekDay <em>Week Day</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.msproject.MsprojectPackage#getWeekDays()
 * @model extendedMetaData="name='WeekDays_._type' kind='elementOnly'"
 * @generated
 */
public interface WeekDays extends EObject {
	/**
	 * Returns the value of the '<em><b>Week Day</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.msproject.WeekDay}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A weekday either defines regular days 
	 *                                             of the week or exception days in the calendar.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Week Day</em>' containment reference list.
	 * @see org.eclipse.epf.msproject.MsprojectPackage#getWeekDays_WeekDay()
	 * @model type="org.eclipse.epf.msproject.WeekDay" containment="true" resolveProxies="false"
	 *        extendedMetaData="kind='element' name='WeekDay' namespace='##targetNamespace'"
	 * @generated
	 */
	EList getWeekDay();

} // WeekDays
