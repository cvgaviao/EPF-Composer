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
import org.eclipse.epf.persistence.util.UnresolvedProxyMarkerManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
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
		if (markderDetailType == null
				|| markderDetailType == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_UNKNOWN) {
			return;
		}
		
		try {
			
			String labelString = (String) selectedMarker.getAttribute(IMarker.MESSAGE);
			String textString = getDialogTextString(selectedMarker, markderDetailType); 

			ShowDetailsProblemViewDialog showDialog = new ShowDetailsProblemViewDialog(
					targetView.getSite().getShell(), labelString, textString);
			showDialog.open();
			
		} catch (Exception ex) {
			AuthoringUIPlugin.getDefault().getLogger().logError(ex);
		}
		
	}

	private String getDialogTextString(IMarker marker, String type) throws Exception {
		StringBuffer sb = new StringBuffer();
		
		String location = (String) marker.getAttribute(IMarker.LOCATION);
		
		//To do: externalize the strings
		if (type == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_FIND_FILE) {
			sb.append("The missing file is referred at the location:\n");
			sb.append(location + "\n\n");
			sb.append("Quick fix will remove this missing file reference from the shown location.");

		} else if (type == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_NORMALIZED_URI ||
				type == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_RESOLVING_PROXY) {
			sb.append("The method element represented by the reference: \n");
			sb.append(marker.getAttribute(UnresolvedProxyMarkerManager.PROXY_URI) + " \n");
			sb.append("can not be resovled.\n\n");
			sb.append("It is referred at the location:\n");
			sb.append(location + "\n\n");
			
			sb.append("Quick fix will remove this unresolved reference from the shown location.");
		} 

		
		return sb.toString();
	}
	
	
	
	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		markderDetailType = null;
		selectedMarker = null;		
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection sel = (IStructuredSelection) selection;			
			boolean b = sel != null && sel.size() == 1;			
			action.setEnabled(b);

			Object selObj = sel.getFirstElement();
			if (selObj instanceof IMarker) {
				selectedMarker = (IMarker) selObj;
				try {		
					markderDetailType = (String) selectedMarker.getAttribute(UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE);
				} catch (Exception ex) {
					AuthoringUIPlugin.getDefault().getLogger().logError(ex);
				}
			}			
		}
	}

}
