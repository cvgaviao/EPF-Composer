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
package org.eclipse.epf.ui.wizards;

import org.eclipse.jface.wizard.IWizardPage;

/**
 * The interface for a replace wizard page contribution.
 * 
 * @author Kelvin Low
 * @since 1.2
 */
public interface IReplaceWizardPageContribution {

	/**
	 * Gets the wizard page that replaces a specific wizard page in a wizard.
	 * 
	 * @return a <code>IWizardPage</code> object
	 */
	public IWizardPage getWizardPage();

	/**
	 * Gets the name of the base wizard page that is should be replaced.
	 * 
	 * @return the name of the referenced base wizard page
	 */
	public String getTargetWizardPage();

}
