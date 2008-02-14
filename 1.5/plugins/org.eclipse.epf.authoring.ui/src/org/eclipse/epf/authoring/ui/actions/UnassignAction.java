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

import java.util.ArrayList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.forms.CustomCategoryAssignPage;
import org.eclipse.epf.authoring.ui.views.LibraryView;
import org.eclipse.epf.library.edit.ui.UserInteractionHelper;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.services.LibraryModificationHelper;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.jface.viewers.IStructuredSelection;

/**
 * Unassign method element.
 * 
 * @author Weiping Lu
 * @since  1.5
 */
public class UnassignAction extends LibraryViewSimpleAction {

	/**
	 * Creates an instance
	 * @param text
	 */
	public UnassignAction(LibraryView libView) {
		super(libView, AuthoringUIResources.unassignAction_text);
	}
	
	protected void doRun() {
		IStructuredSelection selection = (IStructuredSelection) getLibraryView().getSelection();
		Object element = selection.getFirstElement();
		element = TngUtil.unwrap(element);

		if (!(element instanceof CustomCategory)) {
			return;
		}

		CustomCategory category = (CustomCategory) element;

		EObject container = category.eContainer();
		IStatus status = UserInteractionHelper.checkModify(container,
				getLibraryView().getSite().getShell());
		if (container != null && !status.isOK()) {
			AuthoringUIPlugin.getDefault().getMsgDialog().displayError(
					AuthoringUIResources.errorDialog_title,
					AuthoringUIResources.errorDialog_moveError, status);
			return;
		}
		
		Object parent = getSelectionParentObject();
		if (! (parent instanceof CustomCategory)) {
			return;
		}
		CustomCategory parentCc = (CustomCategory) parent;						
		LibraryModificationHelper helper = new LibraryModificationHelper();
		ArrayList elements  = new ArrayList();
		elements.add(element);
		CustomCategoryAssignPage.removeItemsFromModel1(elements, parentCc, new ArrayList(),
				helper.getActionManager(), CustomCategoryAssignPage.getAncestors(parentCc));
	}
	

}
