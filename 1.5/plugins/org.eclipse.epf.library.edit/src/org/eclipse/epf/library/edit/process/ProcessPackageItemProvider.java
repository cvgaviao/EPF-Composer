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
package org.eclipse.epf.library.edit.process;

import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.command.CopyCommand.Helper;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.epf.uma.edit.command.MethodElementCreateCopyCommand;
import org.eclipse.epf.uma.edit.command.MethodElementInitializeCopyCommand;


/**
 * @author Phong Nguyen Le - Sep 30, 2005
 * @since 1.0
 */
public class ProcessPackageItemProvider extends
		org.eclipse.epf.uma.provider.ProcessPackageItemProvider {

	/**
	 * @param adapterFactory
	 */
	public ProcessPackageItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
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
