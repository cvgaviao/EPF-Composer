/**
 * <copyright>
 * </copyright>
 *
 * $Id: DeliverableDescriptionImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
 */
package org.eclipse.epf.xml.uma.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.xml.uma.DeliverableDescription;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Deliverable Description</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.DeliverableDescriptionImpl#getExternalDescription <em>External Description</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.DeliverableDescriptionImpl#getPackagingGuidance <em>Packaging Guidance</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DeliverableDescriptionImpl extends WorkProductDescriptionImpl implements DeliverableDescription {
	/**
	 * The default value of the '{@link #getExternalDescription() <em>External Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalDescription()
	 * @generated
	 * @ordered
	 */
	protected static final String EXTERNAL_DESCRIPTION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getExternalDescription() <em>External Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalDescription()
	 * @generated
	 * @ordered
	 */
	protected String externalDescription = EXTERNAL_DESCRIPTION_EDEFAULT;

	/**
	 * The default value of the '{@link #getPackagingGuidance() <em>Packaging Guidance</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPackagingGuidance()
	 * @generated
	 * @ordered
	 */
	protected static final String PACKAGING_GUIDANCE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPackagingGuidance() <em>Packaging Guidance</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPackagingGuidance()
	 * @generated
	 * @ordered
	 */
	protected String packagingGuidance = PACKAGING_GUIDANCE_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DeliverableDescriptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.DELIVERABLE_DESCRIPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getExternalDescription() {
		return externalDescription;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setExternalDescription(String newExternalDescription) {
		String oldExternalDescription = externalDescription;
		externalDescription = newExternalDescription;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION, oldExternalDescription, externalDescription));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPackagingGuidance() {
		return packagingGuidance;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPackagingGuidance(String newPackagingGuidance) {
		String oldPackagingGuidance = packagingGuidance;
		packagingGuidance = newPackagingGuidance;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE, oldPackagingGuidance, packagingGuidance));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
				return getExternalDescription();
			case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
				return getPackagingGuidance();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
				setExternalDescription((String)newValue);
				return;
			case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
				setPackagingGuidance((String)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(int featureID) {
		switch (featureID) {
			case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
				setExternalDescription(EXTERNAL_DESCRIPTION_EDEFAULT);
				return;
			case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
				setPackagingGuidance(PACKAGING_GUIDANCE_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case UmaPackage.DELIVERABLE_DESCRIPTION__EXTERNAL_DESCRIPTION:
				return EXTERNAL_DESCRIPTION_EDEFAULT == null ? externalDescription != null : !EXTERNAL_DESCRIPTION_EDEFAULT.equals(externalDescription);
			case UmaPackage.DELIVERABLE_DESCRIPTION__PACKAGING_GUIDANCE:
				return PACKAGING_GUIDANCE_EDEFAULT == null ? packagingGuidance != null : !PACKAGING_GUIDANCE_EDEFAULT.equals(packagingGuidance);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (externalDescription: ");
		result.append(externalDescription);
		result.append(", packagingGuidance: ");
		result.append(packagingGuidance);
		result.append(')');
		return result.toString();
	}

} //DeliverableDescriptionImpl