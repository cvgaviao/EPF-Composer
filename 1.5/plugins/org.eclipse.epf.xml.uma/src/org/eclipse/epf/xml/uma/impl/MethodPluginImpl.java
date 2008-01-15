/**
 * <copyright>
 * </copyright>
 *
 * $Id: MethodPluginImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
 */
package org.eclipse.epf.xml.uma.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EDataTypeEList;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.xml.uma.MethodPackage;
import org.eclipse.epf.xml.uma.MethodPlugin;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Method Plugin</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.MethodPluginImpl#getReferencedMethodPlugin <em>Referenced Method Plugin</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.MethodPluginImpl#getMethodPackage <em>Method Package</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.MethodPluginImpl#isUserChangeable <em>User Changeable</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class MethodPluginImpl extends MethodUnitImpl implements MethodPlugin {
	/**
	 * The cached value of the '{@link #getReferencedMethodPlugin() <em>Referenced Method Plugin</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReferencedMethodPlugin()
	 * @generated
	 * @ordered
	 */
	protected EList referencedMethodPlugin;

	/**
	 * The cached value of the '{@link #getMethodPackage() <em>Method Package</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMethodPackage()
	 * @generated
	 * @ordered
	 */
	protected EList methodPackage;

	/**
	 * The default value of the '{@link #isUserChangeable() <em>User Changeable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isUserChangeable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean USER_CHANGEABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUserChangeable() <em>User Changeable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isUserChangeable()
	 * @generated
	 * @ordered
	 */
	protected boolean userChangeable = USER_CHANGEABLE_EDEFAULT;

	/**
	 * This is true if the User Changeable attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean userChangeableESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected MethodPluginImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.METHOD_PLUGIN;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getReferencedMethodPlugin() {
		if (referencedMethodPlugin == null) {
			referencedMethodPlugin = new EDataTypeEList(String.class, this, UmaPackage.METHOD_PLUGIN__REFERENCED_METHOD_PLUGIN);
		}
		return referencedMethodPlugin;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getMethodPackage() {
		if (methodPackage == null) {
			methodPackage = new EObjectContainmentEList(MethodPackage.class, this, UmaPackage.METHOD_PLUGIN__METHOD_PACKAGE);
		}
		return methodPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isUserChangeable() {
		return userChangeable;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setUserChangeable(boolean newUserChangeable) {
		boolean oldUserChangeable = userChangeable;
		userChangeable = newUserChangeable;
		boolean oldUserChangeableESet = userChangeableESet;
		userChangeableESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE, oldUserChangeable, userChangeable, !oldUserChangeableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetUserChangeable() {
		boolean oldUserChangeable = userChangeable;
		boolean oldUserChangeableESet = userChangeableESet;
		userChangeable = USER_CHANGEABLE_EDEFAULT;
		userChangeableESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE, oldUserChangeable, USER_CHANGEABLE_EDEFAULT, oldUserChangeableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetUserChangeable() {
		return userChangeableESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case UmaPackage.METHOD_PLUGIN__METHOD_PACKAGE:
				return ((InternalEList)getMethodPackage()).basicRemove(otherEnd, msgs);
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
			case UmaPackage.METHOD_PLUGIN__REFERENCED_METHOD_PLUGIN:
				return getReferencedMethodPlugin();
			case UmaPackage.METHOD_PLUGIN__METHOD_PACKAGE:
				return getMethodPackage();
			case UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE:
				return isUserChangeable() ? Boolean.TRUE : Boolean.FALSE;
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
			case UmaPackage.METHOD_PLUGIN__REFERENCED_METHOD_PLUGIN:
				getReferencedMethodPlugin().clear();
				getReferencedMethodPlugin().addAll((Collection)newValue);
				return;
			case UmaPackage.METHOD_PLUGIN__METHOD_PACKAGE:
				getMethodPackage().clear();
				getMethodPackage().addAll((Collection)newValue);
				return;
			case UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE:
				setUserChangeable(((Boolean)newValue).booleanValue());
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
			case UmaPackage.METHOD_PLUGIN__REFERENCED_METHOD_PLUGIN:
				getReferencedMethodPlugin().clear();
				return;
			case UmaPackage.METHOD_PLUGIN__METHOD_PACKAGE:
				getMethodPackage().clear();
				return;
			case UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE:
				unsetUserChangeable();
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
			case UmaPackage.METHOD_PLUGIN__REFERENCED_METHOD_PLUGIN:
				return referencedMethodPlugin != null && !referencedMethodPlugin.isEmpty();
			case UmaPackage.METHOD_PLUGIN__METHOD_PACKAGE:
				return methodPackage != null && !methodPackage.isEmpty();
			case UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE:
				return isSetUserChangeable();
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
		result.append(" (referencedMethodPlugin: ");
		result.append(referencedMethodPlugin);
		result.append(", userChangeable: ");
		if (userChangeableESet) result.append(userChangeable); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //MethodPluginImpl