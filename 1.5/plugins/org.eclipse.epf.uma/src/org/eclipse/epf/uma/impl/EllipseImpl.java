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
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getRadiusX <em>Radius X</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getRadiusY <em>Radius Y</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getRotation <em>Rotation</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getStartAngle <em>Start Angle</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getEndAngle <em>End Angle</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.EllipseImpl#getCenter <em>Center</em>}</li>
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
	 * The cached value of the '{@link #getCenter() <em>Center</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCenter()
	 * @generated
	 * @ordered
	 */
	protected Point center;

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
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__RADIUS_X, oldRadiusX, radiusX));
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
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__RADIUS_Y, oldRadiusY, radiusY));
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
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__ROTATION, oldRotation, rotation));
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
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__START_ANGLE, oldStartAngle, startAngle));
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
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__END_ANGLE, oldEndAngle, endAngle));
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
				InternalEObject newCenter = (InternalEObject) center;
				NotificationChain msgs = oldCenter.eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - UmaPackage.ELLIPSE__CENTER,
						null, null);
				if (newCenter.eInternalContainer() == null) {
					msgs = newCenter.eInverseAdd(this, EOPPOSITE_FEATURE_BASE
							- UmaPackage.ELLIPSE__CENTER, null, msgs);
				}
				if (msgs != null)
					msgs.dispatch();
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
	public NotificationChain basicSetCenter(Point newCenter,
			NotificationChain msgs) {
		Point oldCenter = center;
		center = newCenter;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.SET, UmaPackage.ELLIPSE__CENTER, oldCenter,
					newCenter);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCenter(Point newCenter) {
		if (newCenter != center) {
			NotificationChain msgs = null;
			if (center != null)
				msgs = ((InternalEObject) center).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - UmaPackage.ELLIPSE__CENTER,
						null, msgs);
			if (newCenter != null)
				msgs = ((InternalEObject) newCenter).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - UmaPackage.ELLIPSE__CENTER,
						null, msgs);
			msgs = basicSetCenter(newCenter, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.ELLIPSE__CENTER, newCenter, newCenter));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case UmaPackage.ELLIPSE__CENTER:
			return basicSetCenter(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
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
		case UmaPackage.ELLIPSE__CENTER:
			if (resolve)
				return getCenter();
			return basicGetCenter();
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
		case UmaPackage.ELLIPSE__CENTER:
			setCenter((Point) newValue);
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
		case UmaPackage.ELLIPSE__RADIUS_X:
			setRadiusX(RADIUS_X_EDEFAULT);
			return;
		case UmaPackage.ELLIPSE__RADIUS_Y:
			setRadiusY(RADIUS_Y_EDEFAULT);
			return;
		case UmaPackage.ELLIPSE__ROTATION:
			setRotation(ROTATION_EDEFAULT);
			return;
		case UmaPackage.ELLIPSE__START_ANGLE:
			setStartAngle(START_ANGLE_EDEFAULT);
			return;
		case UmaPackage.ELLIPSE__END_ANGLE:
			setEndAngle(END_ANGLE_EDEFAULT);
			return;
		case UmaPackage.ELLIPSE__CENTER:
			setCenter((Point) null);
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
		case UmaPackage.ELLIPSE__RADIUS_X:
			return RADIUS_X_EDEFAULT == null ? radiusX != null
					: !RADIUS_X_EDEFAULT.equals(radiusX);
		case UmaPackage.ELLIPSE__RADIUS_Y:
			return RADIUS_Y_EDEFAULT == null ? radiusY != null
					: !RADIUS_Y_EDEFAULT.equals(radiusY);
		case UmaPackage.ELLIPSE__ROTATION:
			return ROTATION_EDEFAULT == null ? rotation != null
					: !ROTATION_EDEFAULT.equals(rotation);
		case UmaPackage.ELLIPSE__START_ANGLE:
			return START_ANGLE_EDEFAULT == null ? startAngle != null
					: !START_ANGLE_EDEFAULT.equals(startAngle);
		case UmaPackage.ELLIPSE__END_ANGLE:
			return END_ANGLE_EDEFAULT == null ? endAngle != null
					: !END_ANGLE_EDEFAULT.equals(endAngle);
		case UmaPackage.ELLIPSE__CENTER:
			return center != null;
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
		result.append(radiusX);
		result.append(", radiusY: "); //$NON-NLS-1$
		result.append(radiusY);
		result.append(", rotation: "); //$NON-NLS-1$
		result.append(rotation);
		result.append(", startAngle: "); //$NON-NLS-1$
		result.append(startAngle);
		result.append(", endAngle: "); //$NON-NLS-1$
		result.append(endAngle);
		result.append(')');
		return result.toString();
	}

} //EllipseImpl
