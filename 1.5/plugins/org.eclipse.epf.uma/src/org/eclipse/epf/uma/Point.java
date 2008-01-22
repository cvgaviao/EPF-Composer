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

import org.eclipse.epf.uma.ecore.IModelObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Point</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.Point#getX <em>X</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.Point#getY <em>Y</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.epf.uma.UmaPackage#getPoint()
 * @model
 * @extends IModelObject
 * @generated
 */
public interface Point extends IModelObject {
	/**
	 * Returns the value of the '<em><b>X</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>X</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>X</em>' attribute.
	 * @see #isSetX()
	 * @see #unsetX()
	 * @see #setX(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getPoint_X()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getX();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Point#getX <em>X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>X</em>' attribute.
	 * @see #isSetX()
	 * @see #unsetX()
	 * @see #getX()
	 * @generated
	 */
	void setX(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Point#getX <em>X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetX()
	 * @see #getX()
	 * @see #setX(Double)
	 * @generated
	 */
	void unsetX();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Point#getX <em>X</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>X</em>' attribute is set.
	 * @see #unsetX()
	 * @see #getX()
	 * @see #setX(Double)
	 * @generated
	 */
	boolean isSetX();

	/**
	 * Returns the value of the '<em><b>Y</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Y</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Y</em>' attribute.
	 * @see #isSetY()
	 * @see #unsetY()
	 * @see #setY(Double)
	 * @see org.eclipse.epf.uma.UmaPackage#getPoint_Y()
	 * @model unsettable="true" dataType="org.eclipse.epf.uma.Double" required="true" ordered="false"
	 * @generated
	 */
	Double getY();

	/**
	 * Sets the value of the '{@link org.eclipse.epf.uma.Point#getY <em>Y</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Y</em>' attribute.
	 * @see #isSetY()
	 * @see #unsetY()
	 * @see #getY()
	 * @generated
	 */
	void setY(Double value);

	/**
	 * Unsets the value of the '{@link org.eclipse.epf.uma.Point#getY <em>Y</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetY()
	 * @see #getY()
	 * @see #setY(Double)
	 * @generated
	 */
	void unsetY();

	/**
	 * Returns whether the value of the '{@link org.eclipse.epf.uma.Point#getY <em>Y</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Y</em>' attribute is set.
	 * @see #unsetY()
	 * @see #getY()
	 * @see #setY(Double)
	 * @generated
	 */
	boolean isSetY();

} // Point
