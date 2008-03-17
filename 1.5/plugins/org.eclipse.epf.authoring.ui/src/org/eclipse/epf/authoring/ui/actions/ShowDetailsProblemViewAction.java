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
import org.eclipse.epf.authoring.ui.dialogs.ShowDetailsProblemViewDialog;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.views.markers.internal.ProblemView;

/**
 * Show details action on ProblemView
 * 
 * @author Weiping Lu
 * @since 1.5
 *
 */
public class ShowDetailsProblemViewAction implements IViewActionDelegate {

	private ProblemView targetView;
	private IMarker selectedMarker;
	
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
		ShowDetailsProblemViewDialog showDialog = new ShowDetailsProblemViewDialog(targetView.getSite().getShell(),
				"Details to be implemented");
		Text details = showDialog.getDetails();	
		showDialog.open();
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		selectedMarker = null;
		
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection sel = (IStructuredSelection) selection;			
			boolean b = sel != null && sel.size() == 1;			
			action.setEnabled(b);

			Object selObj = sel.getFirstElement();
			if (selObj instanceof IMarker) {
				selectedMarker = (IMarker) selObj;
			}			
		}
	}

}
