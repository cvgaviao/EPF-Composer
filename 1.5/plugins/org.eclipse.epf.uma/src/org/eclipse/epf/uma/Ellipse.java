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
 * A representation of the model object '<em><b>Ellipse</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Ellipse#getCenter <em>Center</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Ellipse#getRadiusX <em>Radius X</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Ellipse#getRadiusY <em>Radius Y</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Ellipse#getRotation <em>Rotation</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Ellipse#getStartAngle <em>Start Angle</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Ellipse#getEndAngle <em>End Angle</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getEllipse()
 * @model
 * @generated
 */
public interface Ellipse extends GraphicPrimitive {
	/**
	 * Returns the value of the '<em><b>Radius X</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Radius X</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Radius X</em>' attribute.
	 * @see #isSetRadiusX()
	 * @see #unsetRadiusX()
	 * @see #setRadiusX(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getEllipse_RadiusX()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getRadiusX();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Ellipse#getRadiusX <em>Radius X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Radius X</em>' attribute.
	 * @see #isSetRadiusX()
	 * @see #unsetRadiusX()
	 * @see #getRadiusX()
	 * @generated
	 */
	void setRadiusX(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Ellipse#getRadiusX <em>Radius X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRadiusX()
	 * @see #getRadiusX()
	 * @see #setRadiusX(Double)
	 * @generated
	 */
	void unsetRadiusX();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Ellipse#getRadiusX <em>Radius X</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Radius X</em>' attribute is set.
	 * @see #unsetRadiusX()
	 * @see #getRadiusX()
	 * @see #setRadiusX(Double)
	 * @generated
	 */
	boolean isSetRadiusX();

	/**
	 * Returns the value of the '<em><b>Radius Y</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Radius Y</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Radius Y</em>' attribute.
	 * @see #isSetRadiusY()
	 * @see #unsetRadiusY()
	 * @see #setRadiusY(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getEllipse_RadiusY()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getRadiusY();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Ellipse#getRadiusY <em>Radius Y</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Radius Y</em>' attribute.
	 * @see #isSetRadiusY()
	 * @see #unsetRadiusY()
	 * @see #getRadiusY()
	 * @generated
	 */
	void setRadiusY(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Ellipse#getRadiusY <em>Radius Y</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRadiusY()
	 * @see #getRadiusY()
	 * @see #setRadiusY(Double)
	 * @generated
	 */
	void unsetRadiusY();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Ellipse#getRadiusY <em>Radius Y</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Radius Y</em>' attribute is set.
	 * @see #unsetRadiusY()
	 * @see #getRadiusY()
	 * @see #setRadiusY(Double)
	 * @generated
	 */
	boolean isSetRadiusY();

	/**
	 * Returns the value of the '<em><b>Rotation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Rotation</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Rotation</em>' attribute.
	 * @see #isSetRotation()
	 * @see #unsetRotation()
	 * @see #setRotation(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getEllipse_Rotation()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getRotation();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Ellipse#getRotation <em>Rotation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Rotation</em>' attribute.
	 * @see #isSetRotation()
	 * @see #unsetRotation()
	 * @see #getRotation()
	 * @generated
	 */
	void setRotation(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Ellipse#getRotation <em>Rotation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRotation()
	 * @see #getRotation()
	 * @see #setRotation(Double)
	 * @generated
	 */
	void unsetRotation();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Ellipse#getRotation <em>Rotation</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Rotation</em>' attribute is set.
	 * @see #unsetRotation()
	 * @see #getRotation()
	 * @see #setRotation(Double)
	 * @generated
	 */
	boolean isSetRotation();

	/**
	 * Returns the value of the '<em><b>Start Angle</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Start Angle</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Start Angle</em>' attribute.
	 * @see #isSetStartAngle()
	 * @see #unsetStartAngle()
	 * @see #setStartAngle(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getEllipse_StartAngle()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getStartAngle();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Ellipse#getStartAngle <em>Start Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Start Angle</em>' attribute.
	 * @see #isSetStartAngle()
	 * @see #unsetStartAngle()
	 * @see #getStartAngle()
	 * @generated
	 */
	void setStartAngle(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Ellipse#getStartAngle <em>Start Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetStartAngle()
	 * @see #getStartAngle()
	 * @see #setStartAngle(Double)
	 * @generated
	 */
	void unsetStartAngle();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Ellipse#getStartAngle <em>Start Angle</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Start Angle</em>' attribute is set.
	 * @see #unsetStartAngle()
	 * @see #getStartAngle()
	 * @see #setStartAngle(Double)
	 * @generated
	 */
	boolean isSetStartAngle();

	/**
	 * Returns the value of the '<em><b>End Angle</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>End Angle</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>End Angle</em>' attribute.
	 * @see #isSetEndAngle()
	 * @see #unsetEndAngle()
	 * @see #setEndAngle(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getEllipse_EndAngle()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getEndAngle();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Ellipse#getEndAngle <em>End Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>End Angle</em>' attribute.
	 * @see #isSetEndAngle()
	 * @see #unsetEndAngle()
	 * @see #getEndAngle()
	 * @generated
	 */
	void setEndAngle(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Ellipse#getEndAngle <em>End Angle</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetEndAngle()
	 * @see #getEndAngle()
	 * @see #setEndAngle(Double)
	 * @generated
	 */
	void unsetEndAngle();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Ellipse#getEndAngle <em>End Angle</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>End Angle</em>' attribute is set.
	 * @see #unsetEndAngle()
	 * @see #getEndAngle()
	 * @see #setEndAngle(Double)
	 * @generated
	 */
	boolean isSetEndAngle();

	/**
	 * Returns the value of the '<em><b>Center</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Center</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Center</em>' reference.
	 * @see #setCenter(Point)
	 * @see org.eclipse.epf.uma.UmaPackage#getEllipse_Center()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Point getCenter();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Ellipse#getCenter <em>Center</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Center</em>' reference.
	 * @see #getCenter()
	 * @generated
	 */
	void setCenter(Point value);

} // Ellipse
