//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.edit.util;

import java.util.Iterator;

import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.Property;
import org.eclipse.epf.uma.UmaFactory;

/**
 * @author Phong Nguyen Le
 *
 * @since 1.2
 */
public class MethodElementPropertyHelper {
	public static final String WORK_ORDER__PREDECESSOR_PROCESS_PATH = "pred_process_path"; //$NON-NLS-1$

	public static final MethodElementProperty getProperty(MethodElement e, String propertyName) {
		if (e != null) {
			for (Iterator iter = e.getMethodElementProperty().iterator(); iter.hasNext();) {
				MethodElementProperty prop = (MethodElementProperty) iter.next();
				if(prop.getName().equals(propertyName)) {
					return prop;
				}
			}
		}
		return null;
	}
	
	public static final void setProperty(MethodElement e, String propName, String propValue) {
		MethodElementProperty prop = getProperty(e, propName);
		if(prop == null) {
			prop = UmaFactory.eINSTANCE.createMethodElementProperty();
			prop.setName(propName);
			e.getMethodElementProperty().add(prop);
		}
		prop.setValue(propValue);
	}

	public static final void setProperty(MethodElement e, String propName, boolean b) {
		setProperty(e, propName, String.valueOf(b));
	}

	public static final void removeProperty(MethodElement e, String propName) {
		MethodElementProperty property = getProperty(e, propName);
		if (property != null) {
			e.getMethodElementProperty().remove(property);
		}
	}
}
