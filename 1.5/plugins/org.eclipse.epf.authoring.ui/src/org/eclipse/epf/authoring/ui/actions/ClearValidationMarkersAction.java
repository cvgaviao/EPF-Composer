/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/
package org.eclipse.epf.authoring.ui.actions;

import org.eclipse.core.resources.IMarker;
import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.validation.ValidationManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.views.markers.MarkerItem;

/**
 * Show explanation action on ProblemView
 * 
 * @author Weiping Lu
 * @since 1.5.1.1
 *
 */
public class ClearValidationMarkersAction implements IViewActionDelegate {
	
	/**
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view) {
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		ValidationManager mgr = (ValidationManager) LibraryEditUtil.getInstance().getValidationManager();
		mgr.clearAllMarkers();
	}


	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
//			action.setEnabled(true);
	}
	
}
