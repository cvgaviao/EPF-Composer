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
 * A representation of the model object '<em><b>Delivery Process Description</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.DeliveryProcessDescription#getScale <em>Scale</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectCharacteristics <em>Project Characteristics</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DeliveryProcessDescription#getRiskLevel <em>Risk Level</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DeliveryProcessDescription#getEstimatingTechnique <em>Estimating Technique</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectMemberExpertise <em>Project Member Expertise</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.DeliveryProcessDescription#getTypeOfContract <em>Type Of Contract</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription()
 * @model
 * @generated
 */
public interface DeliveryProcessDescription extends ProcessDescription {
	/**
	 * Returns the value of the '<em><b>Scale</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Outlines characteristics about the size of a typical project that performs this project expressed in team size, man years, etc.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Scale</em>' attribute.
	 * @see #isSetScale()
	 * @see #unsetScale()
	 * @see #setScale(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription_Scale()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getScale();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getScale <em>Scale</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Scale</em>' attribute.
	 * @see #isSetScale()
	 * @see #unsetScale()
	 * @see #getScale()
	 * @generated
	 */
	void setScale(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getScale <em>Scale</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetScale()
	 * @see #getScale()
	 * @see #setScale(String)
	 * @generated
	 */
	void unsetScale();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getScale <em>Scale</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Scale</em>' attribute is set.
	 * @see #unsetScale()
	 * @see #getScale()
	 * @see #setScale(String)
	 * @generated
	 */
	boolean isSetScale();

	/**
	 * Returns the value of the '<em><b>Project Characteristics</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Characterizes the project that would typically perform this Process
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Project Characteristics</em>' attribute.
	 * @see #isSetProjectCharacteristics()
	 * @see #unsetProjectCharacteristics()
	 * @see #setProjectCharacteristics(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription_ProjectCharacteristics()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getProjectCharacteristics();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectCharacteristics <em>Project Characteristics</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Project Characteristics</em>' attribute.
	 * @see #isSetProjectCharacteristics()
	 * @see #unsetProjectCharacteristics()
	 * @see #getProjectCharacteristics()
	 * @generated
	 */
	void setProjectCharacteristics(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectCharacteristics <em>Project Characteristics</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetProjectCharacteristics()
	 * @see #getProjectCharacteristics()
	 * @see #setProjectCharacteristics(String)
	 * @generated
	 */
	void unsetProjectCharacteristics();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectCharacteristics <em>Project Characteristics</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Project Characteristics</em>' attribute is set.
	 * @see #unsetProjectCharacteristics()
	 * @see #getProjectCharacteristics()
	 * @see #setProjectCharacteristics(String)
	 * @generated
	 */
	boolean isSetProjectCharacteristics();

	/**
	 * Returns the value of the '<em><b>Risk Level</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Outlines typical project risks that are addressed with this process.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Risk Level</em>' attribute.
	 * @see #isSetRiskLevel()
	 * @see #unsetRiskLevel()
	 * @see #setRiskLevel(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription_RiskLevel()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getRiskLevel();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getRiskLevel <em>Risk Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Risk Level</em>' attribute.
	 * @see #isSetRiskLevel()
	 * @see #unsetRiskLevel()
	 * @see #getRiskLevel()
	 * @generated
	 */
	void setRiskLevel(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getRiskLevel <em>Risk Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRiskLevel()
	 * @see #getRiskLevel()
	 * @see #setRiskLevel(String)
	 * @generated
	 */
	void unsetRiskLevel();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getRiskLevel <em>Risk Level</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Risk Level</em>' attribute is set.
	 * @see #unsetRiskLevel()
	 * @see #getRiskLevel()
	 * @see #setRiskLevel(String)
	 * @generated
	 */
	boolean isSetRiskLevel();

	/**
	 * Returns the value of the '<em><b>Estimating Technique</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Describes the Estimation Techniques provided for this Process.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Estimating Technique</em>' attribute.
	 * @see #isSetEstimatingTechnique()
	 * @see #unsetEstimatingTechnique()
	 * @see #setEstimatingTechnique(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription_EstimatingTechnique()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getEstimatingTechnique();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getEstimatingTechnique <em>Estimating Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Estimating Technique</em>' attribute.
	 * @see #isSetEstimatingTechnique()
	 * @see #unsetEstimatingTechnique()
	 * @see #getEstimatingTechnique()
	 * @generated
	 */
	void setEstimatingTechnique(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getEstimatingTechnique <em>Estimating Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetEstimatingTechnique()
	 * @see #getEstimatingTechnique()
	 * @see #setEstimatingTechnique(String)
	 * @generated
	 */
	void unsetEstimatingTechnique();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getEstimatingTechnique <em>Estimating Technique</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Estimating Technique</em>' attribute is set.
	 * @see #unsetEstimatingTechnique()
	 * @see #getEstimatingTechnique()
	 * @see #setEstimatingTechnique(String)
	 * @generated
	 */
	boolean isSetEstimatingTechnique();

	/**
	 * Returns the value of the '<em><b>Project Member Expertise</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Provides a profile of a typical project team, the distribution of roles, skills required for a team performs a project based on this process.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Project Member Expertise</em>' attribute.
	 * @see #isSetProjectMemberExpertise()
	 * @see #unsetProjectMemberExpertise()
	 * @see #setProjectMemberExpertise(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription_ProjectMemberExpertise()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getProjectMemberExpertise();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectMemberExpertise <em>Project Member Expertise</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Project Member Expertise</em>' attribute.
	 * @see #isSetProjectMemberExpertise()
	 * @see #unsetProjectMemberExpertise()
	 * @see #getProjectMemberExpertise()
	 * @generated
	 */
	void setProjectMemberExpertise(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectMemberExpertise <em>Project Member Expertise</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetProjectMemberExpertise()
	 * @see #getProjectMemberExpertise()
	 * @see #setProjectMemberExpertise(String)
	 * @generated
	 */
	void unsetProjectMemberExpertise();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getProjectMemberExpertise <em>Project Member Expertise</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Project Member Expertise</em>' attribute is set.
	 * @see #unsetProjectMemberExpertise()
	 * @see #getProjectMemberExpertise()
	 * @see #setProjectMemberExpertise(String)
	 * @generated
	 */
	boolean isSetProjectMemberExpertise();

	/**
	 * Returns the value of the '<em><b>Type Of Contract</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Provides background information about the contracts that need to be established between a project team that performs this process and a client (e.g. for an IGS engagement).
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Type Of Contract</em>' attribute.
	 * @see #isSetTypeOfContract()
	 * @see #unsetTypeOfContract()
	 * @see #setTypeOfContract(String)
	 * @see org.eclipse.epf.uma.UmaPackage#getDeliveryProcessDescription_TypeOfContract()
	 * @model default="" unsettable="true" dataType="org.eclipse.epf.uma.String" ordered="false"
	 * @generated
	 */
	String getTypeOfContract();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getTypeOfContract <em>Type Of Contract</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type Of Contract</em>' attribute.
	 * @see #isSetTypeOfContract()
	 * @see #unsetTypeOfContract()
	 * @see #getTypeOfContract()
	 * @generated
	 */
	void setTypeOfContract(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getTypeOfContract <em>Type Of Contract</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetTypeOfContract()
	 * @see #getTypeOfContract()
	 * @see #setTypeOfContract(String)
	 * @generated
	 */
	void unsetTypeOfContract();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.DeliveryProcessDescription#getTypeOfContract <em>Type Of Contract</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Type Of Contract</em>' attribute is set.
	 * @see #unsetTypeOfContract()
	 * @see #getTypeOfContract()
	 * @see #setTypeOfContract(String)
	 * @generated
	 */
	boolean isSetTypeOfContract();

} // DeliveryProcessDescription
