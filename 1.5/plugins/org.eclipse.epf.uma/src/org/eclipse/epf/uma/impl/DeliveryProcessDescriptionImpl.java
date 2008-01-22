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
package org.eclipse.epf.uma.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.DeliveryProcessDescription;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Delivery Process Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliveryProcessDescriptionImpl#getScale <em>Scale</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliveryProcessDescriptionImpl#getProjectCharacteristics <em>Project Characteristics</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliveryProcessDescriptionImpl#getRiskLevel <em>Risk Level</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliveryProcessDescriptionImpl#getEstimatingTechnique <em>Estimating Technique</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliveryProcessDescriptionImpl#getProjectMemberExpertise <em>Project Member Expertise</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DeliveryProcessDescriptionImpl#getTypeOfContract <em>Type Of Contract</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DeliveryProcessDescriptionImpl extends ProcessDescriptionImpl
		implements DeliveryProcessDescription {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getScale() <em>Scale</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getScale()
	 * @generated
	 * @ordered
	 */
	protected static final String SCALE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getScale() <em>Scale</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getScale()
	 * @generated
	 * @ordered
	 */
	protected String scale = SCALE_EDEFAULT;

	/**
	 * This is true if the Scale attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean scaleESet;

	/**
	 * The default value of the '{@link #getProjectCharacteristics() <em>Project Characteristics</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProjectCharacteristics()
	 * @generated
	 * @ordered
	 */
	protected static final String PROJECT_CHARACTERISTICS_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getProjectCharacteristics() <em>Project Characteristics</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProjectCharacteristics()
	 * @generated
	 * @ordered
	 */
	protected String projectCharacteristics = PROJECT_CHARACTERISTICS_EDEFAULT;

	/**
	 * This is true if the Project Characteristics attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean projectCharacteristicsESet;

	/**
	 * The default value of the '{@link #getRiskLevel() <em>Risk Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRiskLevel()
	 * @generated
	 * @ordered
	 */
	protected static final String RISK_LEVEL_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getRiskLevel() <em>Risk Level</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRiskLevel()
	 * @generated
	 * @ordered
	 */
	protected String riskLevel = RISK_LEVEL_EDEFAULT;

	/**
	 * This is true if the Risk Level attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean riskLevelESet;

	/**
	 * The default value of the '{@link #getEstimatingTechnique() <em>Estimating Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEstimatingTechnique()
	 * @generated
	 * @ordered
	 */
	protected static final String ESTIMATING_TECHNIQUE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getEstimatingTechnique() <em>Estimating Technique</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEstimatingTechnique()
	 * @generated
	 * @ordered
	 */
	protected String estimatingTechnique = ESTIMATING_TECHNIQUE_EDEFAULT;

	/**
	 * This is true if the Estimating Technique attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean estimatingTechniqueESet;

	/**
	 * The default value of the '{@link #getProjectMemberExpertise() <em>Project Member Expertise</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProjectMemberExpertise()
	 * @generated
	 * @ordered
	 */
	protected static final String PROJECT_MEMBER_EXPERTISE_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getProjectMemberExpertise() <em>Project Member Expertise</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProjectMemberExpertise()
	 * @generated
	 * @ordered
	 */
	protected String projectMemberExpertise = PROJECT_MEMBER_EXPERTISE_EDEFAULT;

	/**
	 * This is true if the Project Member Expertise attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean projectMemberExpertiseESet;

	/**
	 * The default value of the '{@link #getTypeOfContract() <em>Type Of Contract</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTypeOfContract()
	 * @generated
	 * @ordered
	 */
	protected static final String TYPE_OF_CONTRACT_EDEFAULT = "";

	/**
	 * The cached value of the '{@link #getTypeOfContract() <em>Type Of Contract</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTypeOfContract()
	 * @generated
	 * @ordered
	 */
	protected String typeOfContract = TYPE_OF_CONTRACT_EDEFAULT;

	/**
	 * This is true if the Type Of Contract attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean typeOfContractESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DeliveryProcessDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.DELIVERY_PROCESS_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getScale() {
		return scale;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setScale(String newScale) {
		String oldScale = scale;
		scale = newScale;
		boolean oldScaleESet = scaleESet;
		scaleESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__SCALE, oldScale,
					scale, !oldScaleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetScale() {
		String oldScale = scale;
		boolean oldScaleESet = scaleESet;
		scale = SCALE_EDEFAULT;
		scaleESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__SCALE, oldScale,
					SCALE_EDEFAULT, oldScaleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetScale() {
		return scaleESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getProjectCharacteristics() {
		return projectCharacteristics;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setProjectCharacteristics(String newProjectCharacteristics) {
		String oldProjectCharacteristics = projectCharacteristics;
		projectCharacteristics = newProjectCharacteristics;
		boolean oldProjectCharacteristicsESet = projectCharacteristicsESet;
		projectCharacteristicsESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.SET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_CHARACTERISTICS,
					oldProjectCharacteristics, projectCharacteristics,
					!oldProjectCharacteristicsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetProjectCharacteristics() {
		String oldProjectCharacteristics = projectCharacteristics;
		boolean oldProjectCharacteristicsESet = projectCharacteristicsESet;
		projectCharacteristics = PROJECT_CHARACTERISTICS_EDEFAULT;
		projectCharacteristicsESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.UNSET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_CHARACTERISTICS,
					oldProjectCharacteristics,
					PROJECT_CHARACTERISTICS_EDEFAULT,
					oldProjectCharacteristicsESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetProjectCharacteristics() {
		return projectCharacteristicsESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getRiskLevel() {
		return riskLevel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRiskLevel(String newRiskLevel) {
		String oldRiskLevel = riskLevel;
		riskLevel = newRiskLevel;
		boolean oldRiskLevelESet = riskLevelESet;
		riskLevelESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__RISK_LEVEL,
					oldRiskLevel, riskLevel, !oldRiskLevelESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRiskLevel() {
		String oldRiskLevel = riskLevel;
		boolean oldRiskLevelESet = riskLevelESet;
		riskLevel = RISK_LEVEL_EDEFAULT;
		riskLevelESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__RISK_LEVEL,
					oldRiskLevel, RISK_LEVEL_EDEFAULT, oldRiskLevelESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRiskLevel() {
		return riskLevelESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getEstimatingTechnique() {
		return estimatingTechnique;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEstimatingTechnique(String newEstimatingTechnique) {
		String oldEstimatingTechnique = estimatingTechnique;
		estimatingTechnique = newEstimatingTechnique;
		boolean oldEstimatingTechniqueESet = estimatingTechniqueESet;
		estimatingTechniqueESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.SET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__ESTIMATING_TECHNIQUE,
					oldEstimatingTechnique, estimatingTechnique,
					!oldEstimatingTechniqueESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetEstimatingTechnique() {
		String oldEstimatingTechnique = estimatingTechnique;
		boolean oldEstimatingTechniqueESet = estimatingTechniqueESet;
		estimatingTechnique = ESTIMATING_TECHNIQUE_EDEFAULT;
		estimatingTechniqueESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.UNSET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__ESTIMATING_TECHNIQUE,
					oldEstimatingTechnique, ESTIMATING_TECHNIQUE_EDEFAULT,
					oldEstimatingTechniqueESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetEstimatingTechnique() {
		return estimatingTechniqueESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getProjectMemberExpertise() {
		return projectMemberExpertise;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setProjectMemberExpertise(String newProjectMemberExpertise) {
		String oldProjectMemberExpertise = projectMemberExpertise;
		projectMemberExpertise = newProjectMemberExpertise;
		boolean oldProjectMemberExpertiseESet = projectMemberExpertiseESet;
		projectMemberExpertiseESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.SET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_MEMBER_EXPERTISE,
					oldProjectMemberExpertise, projectMemberExpertise,
					!oldProjectMemberExpertiseESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetProjectMemberExpertise() {
		String oldProjectMemberExpertise = projectMemberExpertise;
		boolean oldProjectMemberExpertiseESet = projectMemberExpertiseESet;
		projectMemberExpertise = PROJECT_MEMBER_EXPERTISE_EDEFAULT;
		projectMemberExpertiseESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(
					this,
					Notification.UNSET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_MEMBER_EXPERTISE,
					oldProjectMemberExpertise,
					PROJECT_MEMBER_EXPERTISE_EDEFAULT,
					oldProjectMemberExpertiseESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetProjectMemberExpertise() {
		return projectMemberExpertiseESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getTypeOfContract() {
		return typeOfContract;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTypeOfContract(String newTypeOfContract) {
		String oldTypeOfContract = typeOfContract;
		typeOfContract = newTypeOfContract;
		boolean oldTypeOfContractESet = typeOfContractESet;
		typeOfContractESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__TYPE_OF_CONTRACT,
					oldTypeOfContract, typeOfContract, !oldTypeOfContractESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetTypeOfContract() {
		String oldTypeOfContract = typeOfContract;
		boolean oldTypeOfContractESet = typeOfContractESet;
		typeOfContract = TYPE_OF_CONTRACT_EDEFAULT;
		typeOfContractESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DELIVERY_PROCESS_DESCRIPTION__TYPE_OF_CONTRACT,
					oldTypeOfContract, TYPE_OF_CONTRACT_EDEFAULT,
					oldTypeOfContractESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetTypeOfContract() {
		return typeOfContractESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__SCALE:
			return getScale();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_CHARACTERISTICS:
			return getProjectCharacteristics();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__RISK_LEVEL:
			return getRiskLevel();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__ESTIMATING_TECHNIQUE:
			return getEstimatingTechnique();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_MEMBER_EXPERTISE:
			return getProjectMemberExpertise();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__TYPE_OF_CONTRACT:
			return getTypeOfContract();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__SCALE:
			setScale((String) newValue);
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_CHARACTERISTICS:
			setProjectCharacteristics((String) newValue);
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__RISK_LEVEL:
			setRiskLevel((String) newValue);
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__ESTIMATING_TECHNIQUE:
			setEstimatingTechnique((String) newValue);
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_MEMBER_EXPERTISE:
			setProjectMemberExpertise((String) newValue);
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__TYPE_OF_CONTRACT:
			setTypeOfContract((String) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__SCALE:
			unsetScale();
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_CHARACTERISTICS:
			unsetProjectCharacteristics();
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__RISK_LEVEL:
			unsetRiskLevel();
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__ESTIMATING_TECHNIQUE:
			unsetEstimatingTechnique();
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_MEMBER_EXPERTISE:
			unsetProjectMemberExpertise();
			return;
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__TYPE_OF_CONTRACT:
			unsetTypeOfContract();
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__SCALE:
			return isSetScale();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_CHARACTERISTICS:
			return isSetProjectCharacteristics();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__RISK_LEVEL:
			return isSetRiskLevel();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__ESTIMATING_TECHNIQUE:
			return isSetEstimatingTechnique();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__PROJECT_MEMBER_EXPERTISE:
			return isSetProjectMemberExpertise();
		case UmaPackage.DELIVERY_PROCESS_DESCRIPTION__TYPE_OF_CONTRACT:
			return isSetTypeOfContract();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (scale: "); //$NON-NLS-1$
		if (scaleESet)
			result.append(scale);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", projectCharacteristics: "); //$NON-NLS-1$
		if (projectCharacteristicsESet)
			result.append(projectCharacteristics);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", riskLevel: "); //$NON-NLS-1$
		if (riskLevelESet)
			result.append(riskLevel);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", estimatingTechnique: "); //$NON-NLS-1$
		if (estimatingTechniqueESet)
			result.append(estimatingTechnique);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", projectMemberExpertise: "); //$NON-NLS-1$
		if (projectMemberExpertiseESet)
			result.append(projectMemberExpertise);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", typeOfContract: "); //$NON-NLS-1$
		if (typeOfContractESet)
			result.append(typeOfContract);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //DeliveryProcessDescriptionImpl
