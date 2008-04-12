//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.authoring.ui.actions;

import org.eclipse.core.resources.IMarker;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.dialogs.ShowDetailsProblemViewDialog;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.views.markers.internal.ProblemView;

/**
 * @author Weiping Lu
 * @since 1.5
 * @deprecated
 */
public class ShowDetailsProblemViewAction implements IViewActionDelegate {

	private ProblemView targetView;
	private IMarker selectedMarker;
	private String markderDetailType;
	
	/**
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view) {
		targetView = (ProblemView) view;
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		if (selectedMarker == null) {
			return;
		}
		
		try {
			
			String labelString = (String) selectedMarker.getAttribute(IMarker.MESSAGE);
			String textString = getDialogTextString(selectedMarker, markderDetailType);
			if (textString.length() == 0) {
				return;
			}

			ShowDetailsProblemViewDialog showDialog = new ShowDetailsProblemViewDialog(
					targetView.getSite().getShell(), labelString, textString);
			showDialog.open();
			
		} catch (Exception ex) {
			AuthoringUIPlugin.getDefault().getLogger().logError(ex);
		}
		
	}

	private String getDialogTextString(IMarker marker, String type) throws Exception {
		StringBuffer sb = new StringBuffer();		
		
		return sb.toString();
	}
	
	private String s(String str) {
		return str == null ? "" : str;//$NON-NLS-1$
	}
	
	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		markderDetailType = null;
		selectedMarker = null;		
	}

}
