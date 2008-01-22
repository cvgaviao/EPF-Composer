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

import java.net.URI;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.epf.uma.Classifier;
import org.eclipse.epf.uma.ContentDescription;
import org.eclipse.epf.uma.DescribableElement;
import org.eclipse.epf.uma.Type;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.ContentDescriptionFactory;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Describable Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.DescribableElementImpl#getIsAbstract <em>Is Abstract</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DescribableElementImpl#getPresentation <em>Presentation</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DescribableElementImpl#getShapeicon <em>Shapeicon</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.DescribableElementImpl#getNodeicon <em>Nodeicon</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class DescribableElementImpl extends MethodElementImpl
		implements DescribableElement {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The default value of the '{@link #getIsAbstract() <em>Is Abstract</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsAbstract()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean IS_ABSTRACT_EDEFAULT = Boolean.FALSE;

	/**
	 * The cached value of the '{@link #getIsAbstract() <em>Is Abstract</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsAbstract()
	 * @generated
	 * @ordered
	 */
	protected Boolean isAbstract = IS_ABSTRACT_EDEFAULT;

	/**
	 * This is true if the Is Abstract attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isAbstractESet;

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
	 * The default value of the '{@link #getShapeicon() <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getShapeicon()
	 * @generated
	 * @ordered
	 */
	protected static final URI SHAPEICON_EDEFAULT = (URI) UmaFactory.eINSTANCE
			.createFromString(UmaPackage.eINSTANCE.getUri(), "");

	/**
	 * The cached value of the '{@link #getShapeicon() <em>Shapeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getShapeicon()
	 * @generated
	 * @ordered
	 */
	protected URI shapeicon = SHAPEICON_EDEFAULT;

	/**
	 * This is true if the Shapeicon attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean shapeiconESet;

	/**
	 * The default value of the '{@link #getNodeicon() <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNodeicon()
	 * @generated
	 * @ordered
	 */
	protected static final URI NODEICON_EDEFAULT = (URI) UmaFactory.eINSTANCE
			.createFromString(UmaPackage.eINSTANCE.getUri(), "");

	/**
	 * The cached value of the '{@link #getNodeicon() <em>Nodeicon</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNodeicon()
	 * @generated
	 * @ordered
	 */
	protected URI nodeicon = NODEICON_EDEFAULT;

	/**
	 * This is true if the Nodeicon attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean nodeiconESet;

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
	@Override
	protected EClass eStaticClass() {
		return UmaPackage.Literals.DESCRIBABLE_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getIsAbstract() {
		return isAbstract;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsAbstract(Boolean newIsAbstract) {
		Boolean oldIsAbstract = isAbstract;
		isAbstract = newIsAbstract;
		boolean oldIsAbstractESet = isAbstractESet;
		isAbstractESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT, oldIsAbstract,
					isAbstract, !oldIsAbstractESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsAbstract() {
		Boolean oldIsAbstract = isAbstract;
		boolean oldIsAbstractESet = isAbstractESet;
		isAbstract = IS_ABSTRACT_EDEFAULT;
		isAbstractESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT, oldIsAbstract,
					IS_ABSTRACT_EDEFAULT, oldIsAbstractESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsAbstract() {
		return isAbstractESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetPresentation(
			ContentDescription newPresentation, NotificationChain msgs) {
		ContentDescription oldPresentation = presentation;
		presentation = newPresentation;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.SET,
					UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION,
					oldPresentation, newPresentation);
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
	public URI getShapeicon() {
		return shapeicon;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setShapeicon(URI newShapeicon) {
		URI oldShapeicon = shapeicon;
		shapeicon = newShapeicon;
		boolean oldShapeiconESet = shapeiconESet;
		shapeiconESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON, oldShapeicon,
					shapeicon, !oldShapeiconESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetShapeicon() {
		URI oldShapeicon = shapeicon;
		boolean oldShapeiconESet = shapeiconESet;
		shapeicon = SHAPEICON_EDEFAULT;
		shapeiconESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON, oldShapeicon,
					SHAPEICON_EDEFAULT, oldShapeiconESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetShapeicon() {
		return shapeiconESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public URI getNodeicon() {
		return nodeicon;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setNodeicon(URI newNodeicon) {
		URI oldNodeicon = nodeicon;
		nodeicon = newNodeicon;
		boolean oldNodeiconESet = nodeiconESet;
		nodeiconESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DESCRIBABLE_ELEMENT__NODEICON, oldNodeicon,
					nodeicon, !oldNodeiconESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetNodeicon() {
		URI oldNodeicon = nodeicon;
		boolean oldNodeiconESet = nodeiconESet;
		nodeicon = NODEICON_EDEFAULT;
		nodeiconESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					UmaPackage.DESCRIBABLE_ELEMENT__NODEICON, oldNodeicon,
					NODEICON_EDEFAULT, oldNodeiconESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetNodeicon() {
		return nodeiconESet;
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
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT:
			return getIsAbstract();
		case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
			if (resolve)
				return getPresentation();
			return basicGetPresentation();
		case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
			return getShapeicon();
		case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
			return getNodeicon();
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
		case UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT:
			setIsAbstract((Boolean) newValue);
			return;
		case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
			setPresentation((ContentDescription) newValue);
			return;
		case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
			setShapeicon((URI) newValue);
			return;
		case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
			setNodeicon((URI) newValue);
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
		case UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT:
			unsetIsAbstract();
			return;
		case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
			setPresentation((ContentDescription) null);
			return;
		case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
			unsetShapeicon();
			return;
		case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
			unsetNodeicon();
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
		case UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT:
			return isSetIsAbstract();
		case UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION:
			return presentation != null;
		case UmaPackage.DESCRIBABLE_ELEMENT__SHAPEICON:
			return isSetShapeicon();
		case UmaPackage.DESCRIBABLE_ELEMENT__NODEICON:
			return isSetNodeicon();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eBaseStructuralFeatureID(int derivedFeatureID, Class<?> baseClass) {
		if (baseClass == Type.class) {
			switch (derivedFeatureID) {
			default:
				return -1;
			}
		}
		if (baseClass == Classifier.class) {
			switch (derivedFeatureID) {
			case UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT:
				return UmaPackage.CLASSIFIER__IS_ABSTRACT;
			default:
				return -1;
			}
		}
		return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eDerivedStructuralFeatureID(int baseFeatureID, Class<?> baseClass) {
		if (baseClass == Type.class) {
			switch (baseFeatureID) {
			default:
				return -1;
			}
		}
		if (baseClass == Classifier.class) {
			switch (baseFeatureID) {
			case UmaPackage.CLASSIFIER__IS_ABSTRACT:
				return UmaPackage.DESCRIBABLE_ELEMENT__IS_ABSTRACT;
			default:
				return -1;
			}
		}
		return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
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
		result.append(" (isAbstract: "); //$NON-NLS-1$
		if (isAbstractESet)
			result.append(isAbstract);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", shapeicon: "); //$NON-NLS-1$
		if (shapeiconESet)
			result.append(shapeicon);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(", nodeicon: "); //$NON-NLS-1$
		if (nodeiconESet)
			result.append(nodeicon);
		else
			result.append("<unset>"); //$NON-NLS-1$
		result.append(')');
		return result.toString();
	}

	//
	// Start custom code
	//

	public ContentDescription basicGetPresentation() {
		return presentation;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.epf.uma.impl.DescribableElementImpl#getPresentation()
	 */
	public ContentDescription getPresentation() {
		if (presentation instanceof EObject
				&& ((EObject) presentation).eIsProxy()) {
			ContentDescription oldPresentation = presentation;
			presentation = (ContentDescription) eResolveProxy((InternalEObject) presentation);
			if (presentation != oldPresentation) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION,
							oldPresentation, presentation));
			}
		} else if (presentation == null) {
			//			setPresentation(ContentDescriptionFactory.createContentDescription(this));
			if (emptyPresentation == null) {
				emptyPresentation = ContentDescriptionFactory
						.createContentDescription(this);
				emptyPresentation.eAdapters().add(changeListener);
			}
			return emptyPresentation;
		}

		return presentation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 */
	public void setPresentation(ContentDescription newPresentation) {
		if (newPresentation != presentation) {
			NotificationChain msgs = null;
			if (presentation != null)
				msgs = ((InternalEObject) presentation).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE
								- UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION,
						null, msgs);
			if (newPresentation != null)
				msgs = ((InternalEObject) newPresentation).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE
								- UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION,
						null, msgs);
			msgs = basicSetPresentation(newPresentation, msgs);

			// custom code
			//
			emptyPresentation = null;

			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.DESCRIBABLE_ELEMENT__PRESENTATION,
					newPresentation, newPresentation));
	}

	private ContentDescription emptyPresentation;

	private Adapter changeListener = new AdapterImpl() {
		public void notifyChanged(Notification msg) {
			switch (msg.getEventType()) {
			case Notification.ADD:
			case Notification.ADD_MANY:
			case Notification.SET:
				ContentDescription content = (ContentDescription) getTarget();
				content.eAdapters().remove(this);

				// emptyPresentation is modified, convert it to real presentation if no presentation is set yet 
				// for the ProcessElement
				//
				if (presentation == null) {
					setPresentation(content);
				}
			}
		}
	};
} //DescribableElementImpl
