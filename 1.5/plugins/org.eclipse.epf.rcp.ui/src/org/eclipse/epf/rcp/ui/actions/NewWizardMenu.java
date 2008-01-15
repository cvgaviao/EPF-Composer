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
package org.eclipse.epf.rcp.ui.actions;

import java.util.List;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.BaseNewWizardMenu;

/**
 * Populates the File > New menu with new wizards categorized under
 * "org.eclipse.epf.ui.newWizards.category".
 * 
 * @author Kelvin Low
 * @since 1.2
 */
public class NewWizardMenu extends BaseNewWizardMenu {

	/**
	 * Creates a new wizard shortcut menu for the IDE.
	 * 
	 * @param window
	 *            the window containing the menu
	 * @param id
	 *            the contribution item identifier, or <code>null</code>
	 */
	public NewWizardMenu(IWorkbenchWindow window, String id) {
		super(window, id);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.actions.BaseNewWizardMenu#addItems(org.eclipse.jface.action.IContributionManager)
	 */
	protected void addItems(List list) {
		if (addShortcuts(list)) {
			// Add a separator after the Method COnfiguration menu item.
			if (list.size() > 3) {
				list.add(3, new Separator());
			}
			list.add(new Separator());
		}
		list.add(new ActionContributionItem(getShowDialogAction()));
	}

}
