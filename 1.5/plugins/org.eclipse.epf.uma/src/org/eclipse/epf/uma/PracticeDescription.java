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
 * A representation of the model object '<em><b>Practice Description</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.PracticeDescription#getAdditionalInfo <em>Additional Info</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.PracticeDescription#getProblem <em>Problem</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.PracticeDescription#getBackground <em>Background</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.PracticeDescription#getGoals <em>Goals</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.PracticeDescription#getApplication <em>Application</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.PracticeDescription#getLevelsOfAdoption <em>Levels Of Adoption</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription()
 * @model
 * @generated
 */
public interface PracticeDescription extends ContentDescription {
	/**
	 * Returns the value of the '<em><b>Additional Info</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Any additional Information not covered by the other attributes.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Additional Info</em>' attribute.
	 * @see #isSetAdditionalInfo()
	 * @see #unsetAdditionalInfo()
	 * @see #setAdditionalInfo(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription_AdditionalInfo()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getAdditionalInfo();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getAdditionalInfo <em>Additional Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Additional Info</em>' attribute.
	 * @see #isSetAdditionalInfo()
	 * @see #unsetAdditionalInfo()
	 * @see #getAdditionalInfo()
	 * @generated
	 */
	void setAdditionalInfo(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getAdditionalInfo <em>Additional Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetAdditionalInfo()
	 * @see #getAdditionalInfo()
	 * @see #setAdditionalInfo(String)
	 * @generated
	 */
	void unsetAdditionalInfo();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getAdditionalInfo <em>Additional Info</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Additional Info</em>' attribute is set.
	 * @see #unsetAdditionalInfo()
	 * @see #getAdditionalInfo()
	 * @see #setAdditionalInfo(String)
	 * @generated
	 */
	boolean isSetAdditionalInfo();

	/**
	 * Returns the value of the '<em><b>Problem</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A description of the problem the Practice addresses.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Problem</em>' attribute.
	 * @see #isSetProblem()
	 * @see #unsetProblem()
	 * @see #setProblem(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription_Problem()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getProblem();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getProblem <em>Problem</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Problem</em>' attribute.
	 * @see #isSetProblem()
	 * @see #unsetProblem()
	 * @see #getProblem()
	 * @generated
	 */
	void setProblem(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getProblem <em>Problem</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetProblem()
	 * @see #getProblem()
	 * @see #setProblem(String)
	 * @generated
	 */
	void unsetProblem();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getProblem <em>Problem</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Problem</em>' attribute is set.
	 * @see #unsetProblem()
	 * @see #getProblem()
	 * @see #setProblem(String)
	 * @generated
	 */
	boolean isSetProblem();

	/**
	 * Returns the value of the '<em><b>Background</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Elaboration on the background and the context in which the problem occurs and where the solution described by this Practice will fit in.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Background</em>' attribute.
	 * @see #isSetBackground()
	 * @see #unsetBackground()
	 * @see #setBackground(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription_Background()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getBackground();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getBackground <em>Background</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Background</em>' attribute.
	 * @see #isSetBackground()
	 * @see #unsetBackground()
	 * @see #getBackground()
	 * @generated
	 */
	void setBackground(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getBackground <em>Background</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetBackground()
	 * @see #getBackground()
	 * @see #setBackground(String)
	 * @generated
	 */
	void unsetBackground();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getBackground <em>Background</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Background</em>' attribute is set.
	 * @see #unsetBackground()
	 * @see #getBackground()
	 * @see #setBackground(String)
	 * @generated
	 */
	boolean isSetBackground();

	/**
	 * Returns the value of the '<em><b>Goals</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * A summary of the overall goals to be addressed by the Practice.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Goals</em>' attribute.
	 * @see #isSetGoals()
	 * @see #unsetGoals()
	 * @see #setGoals(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription_Goals()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getGoals();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getGoals <em>Goals</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Goals</em>' attribute.
	 * @see #isSetGoals()
	 * @see #unsetGoals()
	 * @see #getGoals()
	 * @generated
	 */
	void setGoals(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getGoals <em>Goals</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetGoals()
	 * @see #getGoals()
	 * @see #setGoals(String)
	 * @generated
	 */
	void unsetGoals();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getGoals <em>Goals</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Goals</em>' attribute is set.
	 * @see #unsetGoals()
	 * @see #getGoals()
	 * @see #setGoals(String)
	 * @generated
	 */
	boolean isSetGoals();

	/**
	 * Returns the value of the '<em><b>Application</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Describes how the Practice is being applied or introduced into the context described in background.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Application</em>' attribute.
	 * @see #isSetApplication()
	 * @see #unsetApplication()
	 * @see #setApplication(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription_Application()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getApplication();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getApplication <em>Application</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Application</em>' attribute.
	 * @see #isSetApplication()
	 * @see #unsetApplication()
	 * @see #getApplication()
	 * @generated
	 */
	void setApplication(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getApplication <em>Application</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetApplication()
	 * @see #getApplication()
	 * @see #setApplication(String)
	 * @generated
	 */
	void unsetApplication();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getApplication <em>Application</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Application</em>' attribute is set.
	 * @see #unsetApplication()
	 * @see #getApplication()
	 * @see #setApplication(String)
	 * @generated
	 */
	boolean isSetApplication();

	/**
	 * Returns the value of the '<em><b>Levels Of Adoption</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Outlines the different forms or variants in which the practice could be realized. (e.g. full adoption verus a partial adoption of the Practice)
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Levels Of Adoption</em>' attribute.
	 * @see #isSetLevelsOfAdoption()
	 * @see #unsetLevelsOfAdoption()
	 * @see #setLevelsOfAdoption(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getPracticeDescription_LevelsOfAdoption()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getLevelsOfAdoption();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getLevelsOfAdoption <em>Levels Of Adoption</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Levels Of Adoption</em>' attribute.
	 * @see #isSetLevelsOfAdoption()
	 * @see #unsetLevelsOfAdoption()
	 * @see #getLevelsOfAdoption()
	 * @generated
	 */
	void setLevelsOfAdoption(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getLevelsOfAdoption <em>Levels Of Adoption</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetLevelsOfAdoption()
	 * @see #getLevelsOfAdoption()
	 * @see #setLevelsOfAdoption(String)
	 * @generated
	 */
	void unsetLevelsOfAdoption();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.PracticeDescription#getLevelsOfAdoption <em>Levels Of Adoption</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Levels Of Adoption</em>' attribute is set.
	 * @see #unsetLevelsOfAdoption()
	 * @see #getLevelsOfAdoption()
	 * @see #setLevelsOfAdoption(String)
	 * @generated
	 */
	boolean isSetLevelsOfAdoption();

} // PracticeDescription
