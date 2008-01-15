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
package org.eclipse.epf.rcp.ui.actions;

import org.eclipse.epf.rcp.ui.RCPUIPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.IntroAction;

/**
 * Displays the welcome screen for the application.
 * 
 * @author Bingxue Xu
 * @author Kelvin Low
 * @since 1.0
 */
public class IntroductionAction extends IntroAction {

	/**
	 * Creates a new instance.
	 * 
	 * @param window
	 *            the workbench window
	 */
	public IntroductionAction(IWorkbenchWindow window) {
		super(window);
	}

	/**
	 * Gets the image descriptor for this action.
	 * 
	 * @return an image descriptor associated with this action.
	 */
	public ImageDescriptor getImageDescriptor() {
		ImageDescriptor imgDescriptor = RCPUIPlugin.getDefault()
				.getImageDescriptor("full/obj16/product.gif"); //$NON-NLS-1$
		return imgDescriptor;
	}

}
