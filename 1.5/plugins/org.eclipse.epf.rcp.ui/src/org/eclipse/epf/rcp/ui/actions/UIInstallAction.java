/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.rcp.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.update.ui.UpdateManagerUI;

public class UIInstallAction extends Action implements IWorkbenchAction {
	private IWorkbenchWindow window;

	public UIInstallAction(IWorkbenchWindow window) {
		this.window = window;
		setText("Install Software...");
	}
	
	public void run() {
		BusyIndicator.showWhile(window.getShell().getDisplay(),
		        new Runnable() {
		          public void run() {
		            UpdateManagerUI.openInstaller(window.getShell());
		          }
		        });
	}
	
	public void dispose() {
		//
	}

}
