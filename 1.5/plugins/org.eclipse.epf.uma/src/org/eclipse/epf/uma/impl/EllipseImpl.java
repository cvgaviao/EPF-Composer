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
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.Ellipse;
import org.eclipse.epf.uma.Point;
import org.eclipse.epf.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Ellipse</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getCenter <em>Center</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getRadiusX <em>Radius X</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getRadiusY <em>Radius Y</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getRotation <em>Rotation</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getStartAngle <em>Start Angle</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getEndAngle <em>End Angle</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EllipseImpl extends GraphicPrimitiveImpl implements Ellipse {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The cached value of the '{@link #getCenter() <em>Center</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCenter()
	 * @generated
	 * @ordered
	 */
	protected Point center;

	/**
	 * The default value of the '{@link #getRadiusX() <em>Radius X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRadiusX()
	 * @generated
	 * @ordered
	 */
	protected static final Double RADIUS_X_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRadiusX() <em>Radius X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRadiusX()
	 * @generated
	 * @ordered
	 */
	protected Double radiusX = RADIUS_X_EDEFAULT;

	/**
	 * This is true if the Radius X attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean radiusXESet;

	/**
	 * The default value of the '{@link #getRadiusY() <em>Radius Y</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRadiusY()
	 * @generated
	 * @ordered
	 */
	protected static final Double RADIUS_Y_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRadiusY() <em>Radius Y</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRadiusY()
	 * @generated
	 * @ordered
	 */
	protected Double radiusY = RADIUS_Y_EDEFAULT;

	/**
	 * This is true if the Radius Y attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean radiusYESet;

	/**
	 * The default value of the '{@link #getRotation() <em>Rotation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRotation()
	 * @generated
	 * @ordered
	 */
	protected static final Double ROTATION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRotation() <em>Rotation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRotation()
	 * @generated
	 * @ordered
	 */
	protected Double rotation = ROTATION_EDEFAULT;

	/**
	 * This is true if the Rotation attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean rotationESet;

	/**
	 * The default value of the '{@link #getStartAngle() <em>Start Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStartAngle()
	 * @generated
	 * @ordered
	 */
	protected static final Double START_ANGLE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getStartAngle() <em>Start Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStartAngle()
	 * @generated
	 * @ordered
	 */
	protected Double startAngle = START_ANGLE_EDEFAULT;

	/**
	 * This is true if the Start Angle attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean startAngleESet;

	/**
	 * The default value of the '{@link #getEndAngle() <em>End Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEndAngle()
	 * @generated
	 * @ordered
	 */
	protected static final Double END_ANGLE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEndAngle() <em>End Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEndAngle()
	 * @generated
	 * @ordered
	 */
	protected Double endAngle = END_ANGLE_EDEFAULT;

	/**
	 * This is true if the End Angle attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean endAngleESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EllipseImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.ELLIPSE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Double getRadiusX() {
		return radiusX;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRadiusX(Double newRadiusX) {
		Double oldRadiusX = radiusX;
		radiusX = newRadiusX;
		boolean oldRadiusXESet = radiusXESet;
		radiusXESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__RADIUS_X, oldRadiusX, radiusX,
					!oldRadiusXESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRadiusX() {
		Double oldRadiusX = radiusX;
		boolean oldRadiusXESet = radiusXESet;
		radiusX = RADIUS_X_EDEFAULT;
		radiusXESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ELLIPSE__RADIUS_X, oldRadiusX,
					RADIUS_X_EDEFAULT, oldRadiusXESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRadiusX() {
		return radiusXESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Double getRadiusY() {
		return radiusY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRadiusY(Double newRadiusY) {
		Double oldRadiusY = radiusY;
		radiusY = newRadiusY;
		boolean oldRadiusYESet = radiusYESet;
		radiusYESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__RADIUS_Y, oldRadiusY, radiusY,
					!oldRadiusYESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRadiusY() {
		Double oldRadiusY = radiusY;
		boolean oldRadiusYESet = radiusYESet;
		radiusY = RADIUS_Y_EDEFAULT;
		radiusYESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ELLIPSE__RADIUS_Y, oldRadiusY,
					RADIUS_Y_EDEFAULT, oldRadiusYESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRadiusY() {
		return radiusYESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Double getRotation() {
		return rotation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRotation(Double newRotation) {
		Double oldRotation = rotation;
		rotation = newRotation;
		boolean oldRotationESet = rotationESet;
		rotationESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__ROTATION, oldRotation, rotation,
					!oldRotationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRotation() {
		Double oldRotation = rotation;
		boolean oldRotationESet = rotationESet;
		rotation = ROTATION_EDEFAULT;
		rotationESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ELLIPSE__ROTATION, oldRotation,
					ROTATION_EDEFAULT, oldRotationESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRotation() {
		return rotationESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Double getStartAngle() {
		return startAngle;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStartAngle(Double newStartAngle) {
		Double oldStartAngle = startAngle;
		startAngle = newStartAngle;
		boolean oldStartAngleESet = startAngleESet;
		startAngleESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__START_ANGLE, oldStartAngle, startAngle,
					!oldStartAngleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetStartAngle() {
		Double oldStartAngle = startAngle;
		boolean oldStartAngleESet = startAngleESet;
		startAngle = START_ANGLE_EDEFAULT;
		startAngleESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ELLIPSE__START_ANGLE, oldStartAngle,
					START_ANGLE_EDEFAULT, oldStartAngleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetStartAngle() {
		return startAngleESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Double getEndAngle() {
		return endAngle;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEndAngle(Double newEndAngle) {
		Double oldEndAngle = endAngle;
		endAngle = newEndAngle;
		boolean oldEndAngleESet = endAngleESet;
		endAngleESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__END_ANGLE, oldEndAngle, endAngle,
					!oldEndAngleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetEndAngle() {
		Double oldEndAngle = endAngle;
		boolean oldEndAngleESet = endAngleESet;
		endAngle = END_ANGLE_EDEFAULT;
		endAngleESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.ELLIPSE__END_ANGLE, oldEndAngle,
					END_ANGLE_EDEFAULT, oldEndAngleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetEndAngle() {
		return endAngleESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Point getCenter() {
		if (center != null && ((EObject) center).eIsProxy()) {
			InternalEObject oldCenter = (InternalEObject) center;
			center = (Point) eResolveProxy(oldCenter);
			if (center != oldCenter) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.ELLIPSE__CENTER, oldCenter, center));
			}
		}
		return center;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Point basicGetCenter() {
		return center;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCenter(Point newCenter) {
		Point oldCenter = center;
		center = newCenter;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__CENTER, oldCenter, center));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.ELLIPSE__CENTER:
			if (resolve)
				return getCenter();
			return basicGetCenter();
		case UmaPackage.ELLIPSE__RADIUS_X:
			return getRadiusX();
		case UmaPackage.ELLIPSE__RADIUS_Y:
			return getRadiusY();
		case UmaPackage.ELLIPSE__ROTATION:
			return getRotation();
		case UmaPackage.ELLIPSE__START_ANGLE:
			return getStartAngle();
		case UmaPackage.ELLIPSE__END_ANGLE:
			return getEndAngle();
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
		case UmaPackage.ELLIPSE__CENTER:
			setCenter((Point) newValue);
			return;
		case UmaPackage.ELLIPSE__RADIUS_X:
			setRadiusX((Double) newValue);
			return;
		case UmaPackage.ELLIPSE__RADIUS_Y:
			setRadiusY((Double) newValue);
			return;
		case UmaPackage.ELLIPSE__ROTATION:
			setRotation((Double) newValue);
			return;
		case UmaPackage.ELLIPSE__START_ANGLE:
			setStartAngle((Double) newValue);
			return;
		case UmaPackage.ELLIPSE__END_ANGLE:
			setEndAngle((Double) newValue);
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
		case UmaPackage.ELLIPSE__CENTER:
			setCenter((Point) null);
			return;
		case UmaPackage.ELLIPSE__RADIUS_X:
			unsetRadiusX();
			return;
		case UmaPackage.ELLIPSE__RADIUS_Y:
			unsetRadiusY();
			return;
		case UmaPackage.ELLIPSE__ROTATION:
			unsetRotation();
			return;
		case UmaPackage.ELLIPSE__START_ANGLE:
			unsetStartAngle();
			return;
		case UmaPackage.ELLIPSE__END_ANGLE:
			unsetEndAngle();
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
		case UmaPackage.ELLIPSE__CENTER:
			return center != null;
		case UmaPackage.ELLIPSE__RADIUS_X:
			return isSetRadiusX();
		case UmaPackage.ELLIPSE__RADIUS_Y:
			return isSetRadiusY();
		case UmaPackage.ELLIPSE__ROTATION:
			return isSetRotation();
		case UmaPackage.ELLIPSE__START_ANGLE:
			return isSetStartAngle();
		case UmaPackage.ELLIPSE__END_ANGLE:
			return isSetEndAngle();
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
		result.append(" (radiusX: "); //$NON-NLS-1$
		if (radiusXESet)
			result.append(radiusX);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", radiusY: "); //$NON-NLS-1$
		if (radiusYESet)
			result.append(radiusY);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", rotation: "); //$NON-NLS-1$
		if (rotationESet)
			result.append(rotation);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", startAngle: "); //$NON-NLS-1$
		if (startAngleESet)
			result.append(startAngle);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", endAngle: "); //$NON-NLS-1$
		if (endAngleESet)
			result.append(endAngle);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

} //EllipseImpl
