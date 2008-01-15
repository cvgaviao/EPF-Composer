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
package org.eclipse.epf.uma.ecore.util;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;

/**
 * @author Phong Nguyen Le
 * @since 1.0
 * @deprecated
 */
public class EObjectContainmentResolvingEList extends EObjectContainmentEList {

	private static final long serialVersionUID = 5349833630551976488L;

	public static class Unsettable extends EObjectContainmentResolvingEList {

		private static final long serialVersionUID = -5117004151476522719L;

		protected boolean isSet;

		public Unsettable(Class dataClass, InternalEObject owner, int featureID) {
			super(dataClass, owner, featureID);
		}

		protected void didChange() {
			isSet = true;
		}

		public boolean isSet() {
			return isSet;
		}

		public void unset() {
			super.unset();
			if (isNotificationRequired()) {
				boolean oldIsSet = isSet;
				isSet = false;
				owner.eNotify(createNotification(Notification.UNSET, oldIsSet,
						false));
			} else {
				isSet = false;
			}
		}
	}

	public EObjectContainmentResolvingEList(Class dataClass,
			InternalEObject owner, int featureID) {
		super(dataClass, owner, featureID);
	}

	protected boolean hasProxies() {
		return true;
	}

	/**
	 * @see org.eclipse.emf.common.util.BasicEList#resolve(int,
	 *      java.lang.Object)
	 */
	protected Object resolve(int index, Object object) {
		MultiResourceEObject eObj = (MultiResourceEObject) resolve(index,
				(EObject) object);
		eObj.eSetContainer(owner, InternalEObject.EOPPOSITE_FEATURE_BASE
				- getFeatureID());
		return eObj;
	}

	/**
	 * @see org.eclipse.emf.ecore.util.EcoreEList#contains(java.lang.Object)
	 */
	public boolean contains(Object object) {
		if (isEObject()) {
			boolean result = super.contains(object);
			if (hasProxies() && !result) {
				for (int i = 0; i < size; ++i) {
					EObject eObject = resolveProxy((EObject) data[i]);
					if (eObject == object) {
						return true;
					}
				}
			}
			return result;
		}
		return super.contains(object);
	}

}
