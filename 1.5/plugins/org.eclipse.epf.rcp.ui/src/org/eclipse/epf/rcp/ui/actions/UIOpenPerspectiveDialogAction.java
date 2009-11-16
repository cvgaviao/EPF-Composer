//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/eplv10.html
//
// Contributors:
// IBM Corporation  initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.rcp.ui.actions;

import org.eclipse.epf.common.ui.util.PerspectiveUtil;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.OpenPreferencesAction;
import org.eclipse.ui.internal.dialogs.SelectPerspectiveDialog;

/**
 * Defines the class.
 * 
 * @author Bingxue Xu
 * @since 1.0
 */

public class UIOpenPerspectiveDialogAction extends OpenPreferencesAction {
	private IWorkbenchWindow workbenchWindow;

	/**
	 * Constructor
	 * @param window
	 */
	public UIOpenPerspectiveDialogAction(IWorkbenchWindow window) {
		super(window);
		this.workbenchWindow = window;
	}

	 /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
	public void run() {
		if (workbenchWindow == null) {
			return;
		}
		SelectPerspectiveDialog dlg = new SelectPerspectiveDialog(
				workbenchWindow.getShell(), workbenchWindow.getWorkbench()
						.getPerspectiveRegistry());
		dlg.open();
		if (dlg.getReturnCode() == Window.CANCEL)
			return;
		IPerspectiveDescriptor desc = dlg.getSelection();
		if (desc != null) {
			PerspectiveUtil.openPerspective(desc.getId());
		}
	}

	public void dispose() {
		workbenchWindow = null;
		super.dispose();
	}

}