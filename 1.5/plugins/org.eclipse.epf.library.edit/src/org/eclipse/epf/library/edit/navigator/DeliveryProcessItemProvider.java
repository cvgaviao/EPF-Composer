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
package org.eclipse.epf.library.edit.navigator;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.command.CopyCommand.Helper;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.edit.command.MethodElementCreateCopyCommand;
import org.eclipse.epf.uma.edit.command.MethodElementInitializeCopyCommand;

/**
 * The item provider adapter for a delivery process in the Library view.
 * 
 * @author Phong Nguyen Le
 * @author Kelvin Low
 * @since 1.0
 */
public class DeliveryProcessItemProvider extends
		org.eclipse.epf.uma.provider.DeliveryProcessItemProvider {

	/**
	 * Creates a new instance.
	 */
	public DeliveryProcessItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}

	protected void collectNewChildDescriptors(Collection newChildDescriptors,
			Object object) {

	}

	public Collection getChildrenFeatures(Object object) {
		return Collections.EMPTY_LIST;
	}

	public String getText(Object object) {
		return TngUtil.getLabel(object, getString("_UI_DeliveryProcess_type")); //$NON-NLS-1$
	}

	protected Command createInitializeCopyCommand(EditingDomain domain,
			EObject owner, Helper helper) {
		return new MethodElementInitializeCopyCommand(domain, owner, helper);
	}

	protected Command createCreateCopyCommand(EditingDomain domain,
			EObject owner, Helper helper) {
		return new MethodElementCreateCopyCommand(domain, owner, helper);
	}

}
