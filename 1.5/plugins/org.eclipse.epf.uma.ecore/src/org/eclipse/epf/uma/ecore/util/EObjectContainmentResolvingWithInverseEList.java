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
import org.eclipse.emf.ecore.InternalEObject;

/**
 * @author Phong Nguyen Le
 * @since 1.0
 * @deprecated
 */
public class EObjectContainmentResolvingWithInverseEList extends
		EObjectContainmentResolvingEList {

	private static final long serialVersionUID = -122177081657171857L;

	public static class Unsettable extends
			EObjectContainmentResolvingWithInverseEList {

		private static final long serialVersionUID = -8239294568676270078L;

		protected boolean isSet;

		public Unsettable(Class dataClass, InternalEObject owner,
				int featureID, int inverseFeatureID) {
			super(dataClass, owner, featureID, inverseFeatureID);
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

	protected final int inverseFeatureID;

	public EObjectContainmentResolvingWithInverseEList(Class dataClass,
			InternalEObject owner, int featureID, int inverseFeatureID) {
		super(dataClass, owner, featureID);
		this.inverseFeatureID = inverseFeatureID;
	}

	protected boolean hasNavigableInverse() {
		return true;
	}

	public int getInverseFeatureID() {
		return inverseFeatureID;
	}

	public Class getInverseFeatureClass() {
		return dataClass;
	}

}
