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

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Activity Description</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.ActivityDescription#getPurpose <em>Purpose</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ActivityDescription#getAlternatives <em>Alternatives</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.ActivityDescription#getHowtoStaff <em>Howto Staff</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getActivityDescription()
 * @model
 * @generated
 */
public interface ActivityDescription extends BreakdownElementDescription {
	/**
	 * Returns the value of the '<em><b>Purpose</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Summarizes the main reason for performing this Activity, describes what the activity as a whole is intended to achieve.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Purpose</em>' attribute.
	 * @see #isSetPurpose()
	 * @see #unsetPurpose()
	 * @see #setPurpose(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getActivityDescription_Purpose()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getPurpose();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getPurpose <em>Purpose</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Purpose</em>' attribute.
	 * @see #isSetPurpose()
	 * @see #unsetPurpose()
	 * @see #getPurpose()
	 * @generated
	 */
	void setPurpose(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getPurpose <em>Purpose</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetPurpose()
	 * @see #getPurpose()
	 * @see #setPurpose(String)
	 * @generated
	 */
	void unsetPurpose();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getPurpose <em>Purpose</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Purpose</em>' attribute is set.
	 * @see #unsetPurpose()
	 * @see #getPurpose()
	 * @see #setPurpose(String)
	 * @generated
	 */
	boolean isSetPurpose();

	/**
	 * Returns the value of the '<em><b>Alternatives</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Comprises of summaries describing important exceptional and non-standard ways of doing the work of this Activity not covered by the Activity's Tasks.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Alternatives</em>' attribute.
	 * @see #isSetAlternatives()
	 * @see #unsetAlternatives()
	 * @see #setAlternatives(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getActivityDescription_Alternatives()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getAlternatives();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getAlternatives <em>Alternatives</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Alternatives</em>' attribute.
	 * @see #isSetAlternatives()
	 * @see #unsetAlternatives()
	 * @see #getAlternatives()
	 * @generated
	 */
	void setAlternatives(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getAlternatives <em>Alternatives</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetAlternatives()
	 * @see #getAlternatives()
	 * @see #setAlternatives(String)
	 * @generated
	 */
	void unsetAlternatives();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getAlternatives <em>Alternatives</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Alternatives</em>' attribute is set.
	 * @see #unsetAlternatives()
	 * @see #getAlternatives()
	 * @see #setAlternatives(String)
	 * @generated
	 */
	boolean isSetAlternatives();

	/**
	 * Returns the value of the '<em><b>Howto Staff</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Provides background on who should be involved in this activity what are the required skills, experience,  and perhaps attitudes.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Howto Staff</em>' attribute.
	 * @see #isSetHowtoStaff()
	 * @see #unsetHowtoStaff()
	 * @see #setHowtoStaff(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getActivityDescription_HowtoStaff()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getHowtoStaff();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getHowtoStaff <em>Howto Staff</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Howto Staff</em>' attribute.
	 * @see #isSetHowtoStaff()
	 * @see #unsetHowtoStaff()
	 * @see #getHowtoStaff()
	 * @generated
	 */
	void setHowtoStaff(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getHowtoStaff <em>Howto Staff</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetHowtoStaff()
	 * @see #getHowtoStaff()
	 * @see #setHowtoStaff(String)
	 * @generated
	 */
	void unsetHowtoStaff();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.ActivityDescription#getHowtoStaff <em>Howto Staff</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Howto Staff</em>' attribute is set.
	 * @see #unsetHowtoStaff()
	 * @see #getHowtoStaff()
	 * @see #setHowtoStaff(String)
	 * @generated
	 */
	boolean isSetHowtoStaff();

} // ActivityDescription
