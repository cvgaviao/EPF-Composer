/**
 * <copyright>
 * </copyright>
 *
 * $Id: Calendars.java,v 1.1 2008/01/15 08:52:45 jtham Exp $
 */
package org.eclipse.epf.msproject;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Calendars</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.msproject.Calendars#getCalendar <em>Calendar</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.msproject.MsprojectPackage#getCalendars()
 * @model extendedMetaData="name='Calendars_._type' kind='elementOnly'"
 * @generated
 */
public interface Calendars extends EObject {
	/**
	 * Returns the value of the '<em><b>Calendar</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.epf.msproject.Calendar}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Calendars are used to define standard working and 
	 *                     non-working times. Projects must have one base calendar. Tasks and resources 
	 *                     may have their own non-base calendars which are based on a base calendar.
	 *                     
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Calendar</em>' containment reference list.
	 * @see org.eclipse.epf.msproject.MsprojectPackage#getCalendars_Calendar()
	 * @model type="org.eclipse.epf.msproject.Calendar" containment="true" resolveProxies="false" required="true"
	 *        extendedMetaData="kind='element' name='Calendar' namespace='##targetNamespace'"
	 * @generated
	 */
	EList getCalendar();

} // Calendars
