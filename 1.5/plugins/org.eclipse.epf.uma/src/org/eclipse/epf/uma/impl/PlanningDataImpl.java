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

import java.util.Date;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.PlanningData;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Planning Data</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.PlanningDataImpl#getStartDate <em>Start Date</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PlanningDataImpl#getFinishDate <em>Finish Date</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.PlanningDataImpl#getRank <em>Rank</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PlanningDataImpl extends ProcessElementImpl implements
		PlanningData {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getStartDate() <em>Start Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStartDate()
	 * @generated
	 * @ordered
	 */
	protected static final Date START_DATE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getStartDate() <em>Start Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStartDate()
	 * @generated
	 * @ordered
	 */
	protected Date startDate = START_DATE_EDEFAULT;

	/**
	 * This is true if the Start Date attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean startDateESet;

	/**
	 * The default value of the '{@link #getFinishDate() <em>Finish Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFinishDate()
	 * @generated
	 * @ordered
	 */
	protected static final Date FINISH_DATE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFinishDate() <em>Finish Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFinishDate()
	 * @generated
	 * @ordered
	 */
	protected Date finishDate = FINISH_DATE_EDEFAULT;

	/**
	 * This is true if the Finish Date attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean finishDateESet;

	/**
	 * The default value of the '{@link #getRank() <em>Rank</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRank()
	 * @generated
	 * @ordered
	 */
	protected static final int RANK_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getRank() <em>Rank</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRank()
	 * @generated
	 * @ordered
	 */
	protected int rank = RANK_EDEFAULT;

	/**
	 * This is true if the Rank attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean rankESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected PlanningDataImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.PLANNING_DATA;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Date getStartDate() {
		return startDate;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStartDate(Date newStartDate) {
		Date oldStartDate = startDate;
		startDate = newStartDate;
		boolean oldStartDateESet = startDateESet;
		startDateESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PLANNING_DATA__START_DATE, oldStartDate,
					startDate, !oldStartDateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetStartDate() {
		Date oldStartDate = startDate;
		boolean oldStartDateESet = startDateESet;
		startDate = START_DATE_EDEFAULT;
		startDateESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PLANNING_DATA__START_DATE, oldStartDate,
					START_DATE_EDEFAULT, oldStartDateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetStartDate() {
		return startDateESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Date getFinishDate() {
		return finishDate;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setFinishDate(Date newFinishDate) {
		Date oldFinishDate = finishDate;
		finishDate = newFinishDate;
		boolean oldFinishDateESet = finishDateESet;
		finishDateESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PLANNING_DATA__FINISH_DATE, oldFinishDate,
					finishDate, !oldFinishDateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetFinishDate() {
		Date oldFinishDate = finishDate;
		boolean oldFinishDateESet = finishDateESet;
		finishDate = FINISH_DATE_EDEFAULT;
		finishDateESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PLANNING_DATA__FINISH_DATE, oldFinishDate,
					FINISH_DATE_EDEFAULT, oldFinishDateESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetFinishDate() {
		return finishDateESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getRank() {
		return rank;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRank(int newRank) {
		int oldRank = rank;
		rank = newRank;
		boolean oldRankESet = rankESet;
		rankESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.PLANNING_DATA__RANK, oldRank, rank, !oldRankESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRank() {
		int oldRank = rank;
		boolean oldRankESet = rankESet;
		rank = RANK_EDEFAULT;
		rankESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.PLANNING_DATA__RANK, oldRank, RANK_EDEFAULT,
					oldRankESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRank() {
		return rankESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.PLANNING_DATA__START_DATE:
			return getStartDate();
		case UmaPackage.PLANNING_DATA__FINISH_DATE:
			return getFinishDate();
		case UmaPackage.PLANNING_DATA__RANK:
			return new Integer(getRank());
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
		case UmaPackage.PLANNING_DATA__START_DATE:
			setStartDate((Date) newValue);
			return;
		case UmaPackage.PLANNING_DATA__FINISH_DATE:
			setFinishDate((Date) newValue);
			return;
		case UmaPackage.PLANNING_DATA__RANK:
			setRank(((Integer) newValue).intValue());
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
		case UmaPackage.PLANNING_DATA__START_DATE:
			unsetStartDate();
			return;
		case UmaPackage.PLANNING_DATA__FINISH_DATE:
			unsetFinishDate();
			return;
		case UmaPackage.PLANNING_DATA__RANK:
			unsetRank();
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
		case UmaPackage.PLANNING_DATA__START_DATE:
			return isSetStartDate();
		case UmaPackage.PLANNING_DATA__FINISH_DATE:
			return isSetFinishDate();
		case UmaPackage.PLANNING_DATA__RANK:
			return isSetRank();
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
		result.append(" (startDate: "); //$NON-NLS-1$
		if (startDateESet)
			result.append(startDate);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", finishDate: "); //$NON-NLS-1$
		if (finishDateESet)
			result.append(finishDate);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", rank: "); //$NON-NLS-1$
		if (rankESet)
			result.append(rank);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //PlanningDataImpl
