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
import org.eclipse.epf.authoring.ui.util.ConfigurationMarkerHelper;
import org.eclipse.epf.library.LibraryResources;
import org.eclipse.epf.persistence.util.UnresolvedProxyMarkerManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.views.markers.MarkerViewUtil;
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
			try {
				if (! selectedMarker.getType().equals(
						ConfigurationMarkerHelper.MARKER_ID)) {
					return;
				}
			} catch (Exception e) {
				return;
			}
		}
		
		ConfigurationMarkerHelper a;
		
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
		
		String location = (String) marker.getAttribute(IMarker.LOCATION);
		String indent = "     ";
		sb.append("Problem cause\n");
		
		//To do: externalize the strings
		if (type == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_FIND_FILE) {
			sb.append(indent + "An xmi file is referring to another file in the library,\n");
			sb.append(indent + "but the referred to file is missing.\n");		
			sb.append("\nMissing file\n");
			sb.append(indent + "The location is shown in the error message description.\n");
			sb.append("\nReferring xmi file\n");
			sb.append(indent + "Location: " + location + "\n");
			sb.append("\nQuick fix\n");
			sb.append(indent + "Will remove the missing file reference from referring xmi file.");

		} else if (type == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_NORMALIZED_URI ||
				type == UnresolvedProxyMarkerManager.MARKER_DETAIL_TYPE_RESOLVING_PROXY) {
			sb.append(indent + "An xmi file is referring to a method element represented by a proxy URI,\n");
			sb.append(indent + "but the referred to method element cannot be found by resolving the proxy URI.\n");

			sb.append("\nReferring xmi file\n"); 
			sb.append(indent + "Location: " + location + "\n");
			
			sb.append("\nReferred to method element\n"); 
			sb.append(indent + "Proxy URI: " + marker.getAttribute(UnresolvedProxyMarkerManager.PROXY_URI) + " \n");			
			
			sb.append("\nQuick fix\n");
			sb.append(indent + "Will remove the referred to method element reference from referring xmi file.");
			
		} else if (marker.getType().equals(ConfigurationMarkerHelper.MARKER_ID)) {
			String messageId = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_MESSAGE_ID);			
			String elementName = (String) marker.getAttribute(MarkerViewUtil.NAME_ATTRIBUTE);
			String causeName = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_CAUSE_ELEMENT_NAME);
			String elementGuid = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_ERROR_ELEMENT_GUID);
			String causeGuid = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_CAUSE_ELEMENT_GUID);
			String elementType = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_ERROR_ELEMENT_TYPE);
			String causeType = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_CAUSE_ELEMENT_TYPE);									
			String causeLocation = (String) marker.getAttribute(ConfigurationMarkerHelper.ATTR_CAUSE_ELEMENT_LOCATION);
			
			sb.append(indent + "A method element is referring to another method element.\n");
			sb.append(indent + "The referring method element is in the configuration, \n");
			sb.append(indent + "but the referred to method element is not.\n");

			sb.append("\nReferring method element\n");
			sb.append(indent + "type: " + elementType + "\n"); 
			sb.append(indent + "name, location: " + location + "\n");
			sb.append(indent + "guid: " + elementGuid + "\n");
			
			sb.append("\nRefered to method element\n");
			sb.append(indent + "type: " + causeType + "\n"); 
			sb.append(indent + "name, location: " + causeLocation + "\n");
			sb.append(indent + "guid: " + causeGuid + "\n");	

			String elementRole = null;
			String causeRole = null;
			
			if (messageId == LibraryResources.ElementError_contributor_missing_base) {
				elementRole = "Variability contributor";
				causeRole = "Variability base";
			} else if (messageId == LibraryResources.ElementError_extender_missing_base) {
				elementRole = "Variability extender";
				causeRole = "Variability base";
			} else if (messageId == LibraryResources.ElementError_replacer_missing_base) {
				elementRole = "Variability replacer";
				causeRole = "Variability base";
			} else if (messageId == LibraryResources.ElementError_missing_primary_performer) {
				elementRole = "Task";
				causeRole = "Primary performer";
			} else if (messageId == LibraryResources.ElementError_missing_mandatory_input) {
				elementRole = "Task";
				causeRole = "Mandatory input work product";
			} else if (messageId == LibraryResources.ElementError_missing_output) {
				elementRole = "Task";
				causeRole = "Output work product";
			} else if (messageId == LibraryResources.ElementError_missing_responsible_for_workProduct) {
				elementRole = "Role";
				causeRole = "Respossible for work product";
			} else if (messageId == LibraryResources.ElementError_missing_element) {
				
			}
			
			if (elementRole != null && causeRole != null) {
				sb.append("\nRelationship\n");
				sb.append(indent + "Referring method element: " + elementRole + "\n"); 
				sb.append(indent + "Referred to method element: " + causeRole + "\n"); 
			}
			
			sb.append("\nQuick fix\n");
			sb.append(indent + "Will add the referred to method element inclduing its containing package to the configuration.");

		}

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
