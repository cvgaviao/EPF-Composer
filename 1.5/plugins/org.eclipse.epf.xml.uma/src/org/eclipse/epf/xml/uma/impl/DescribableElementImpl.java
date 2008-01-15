/**
 * <copyright>
 * </copyright>
 *
 * $Id: DescribableElementImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
 */
package org.eclipse.epf.xml.uma.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.xml.uma.ContentDescription;
import org.eclipse.epf.xml.uma.DescribableElement;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Describable Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.DescribableElementImpl#getPresentation <em>Presentation</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.DescribableElementImpl#getNodeicon <em>Nodeicon</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.DescribableElementImpl#getPresentationName <em>Presentation Name</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.DescribableElementImpl#getShapeicon <em>Shapeicon</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DescribableElementImpl extends MethodElementImpl implements DescribableElement {
	/**
	 * The cached value of the '{@link #getPresentation() <em>Presentation</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentation()
	 * @generated
	 * @ordered
	 */
	protected ContentDescription presentation;

	/**
	 * The default value of the '{@link #getNodeicon() <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNodeicon()
	 * @generated
	 * @ordered
	 */
	protected static final String NODEICON_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getNodeicon() <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNodeicon()
	 * @generated
	 * @ordered
	 */
	protected String nodeicon = NODEICON_EDEFAULT;

	/**
	 * The default value of the '{@link #getPresentationName() <em>Presentation Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentationName()
	 * @generated
	 * @ordered
	 */
	protected static final String PRESENTATION_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPresentationName() <em>Presentation Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPresentationName()
	 * @generated
	 * @ordered
	 */
	protected String presentationName = PRESENTATION_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getShapeicon() <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getShapeicon()
	 * @generated
	 * @ordered
	 */
	protected static final String SHAPEICON_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getShapeicon() <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getShapeicon()
	 * @generated
	 * @ordered
	 */
	protected String shapeicon = SHAPEICON_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DescribableElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.DESCRIBABLE_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ContentDescription getPresentation() {
		return presentation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetPresentation(ContentDescription newPresentation, NotificationChain msgs) {
		ContentDescription oldPresentation = presentation;
		presentation = newPresentation;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION, oldPresentation, newPresentation);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPresentation(ContentDescription newPresentation) {
		if (newPresentation != presentation) {
			NotificationChain msgs = null;
			if (presentation != null)
				msgs = ((InternalEObject)presentation).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION, null, msgs);
			if (newPresentation != null)
				msgs = ((InternalEObject)newPresentation).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION, null, msgs);
			msgs = basicSetPresentation(newPresentation, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION, newPresentation, newPresentation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getNodeicon() {
		return nodeicon;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setNodeicon(String newNodeicon) {
		String oldNodeicon = nodeicon;
		nodeicon = newNodeicon;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.DESCRIBABLE_ELEMENT__NODEICON, oldNodeicon, nodeicon));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPresentationName() {
		return presentationName;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPresentationName(String newPresentationName) {
		String oldPresentationName = presentationName;
		presentationName = newPresentationName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION_NAME, oldPresentationName, presentationName));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getShapeicon() {
		return shapeicon;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setShapeicon(String newShapeicon) {
		String oldShapeicon = shapeicon;
		shapeicon = newShapeicon;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON, oldShapeicon, shapeicon));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
				return basicSetPresentation(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
				return getPresentation();
			case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
				return getNodeicon();
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION_NAME:
				return getPresentationName();
			case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
				return getShapeicon();
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
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
				setPresentation((ContentDescription)newValue);
				return;
			case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
				setNodeicon((String)newValue);
				return;
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION_NAME:
				setPresentationName((String)newValue);
				return;
			case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
				setShapeicon((String)newValue);
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
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
				setPresentation((ContentDescription)null);
				return;
			case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
				setNodeicon(NODEICON_EDEFAULT);
				return;
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION_NAME:
				setPresentationName(PRESENTATION_NAME_EDEFAULT);
				return;
			case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
				setShapeicon(SHAPEICON_EDEFAULT);
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
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
				return presentation != null;
			case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
				return NODEICON_EDEFAULT == null ? nodeicon != null : !NODEICON_EDEFAULT.equals(nodeicon);
			case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION_NAME:
				return PRESENTATION_NAME_EDEFAULT == null ? presentationName != null : !PRESENTATION_NAME_EDEFAULT.equals(presentationName);
			case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
				return SHAPEICON_EDEFAULT == null ? shapeicon != null : !SHAPEICON_EDEFAULT.equals(shapeicon);
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
		result.append(" (nodeicon: ");
		result.append(nodeicon);
		result.append(", presentationName: ");
		result.append(presentationName);
		result.append(", shapeicon: ");
		result.append(shapeicon);
		result.append(')');
		return result.toString();
	}

} //DescribableElementImpl