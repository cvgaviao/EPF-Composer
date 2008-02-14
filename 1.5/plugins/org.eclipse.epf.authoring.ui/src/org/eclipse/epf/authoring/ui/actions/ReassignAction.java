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

import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.views.LibraryView;

/**
 * Reassign method element.
 * 
 * @author Weiping Lu
 * @since  1.5
 */
public class ReassignAction extends LibraryViewSimpleAction {

	/**
	 * Creates an instance
	 * @param text
	 */
	public ReassignAction(LibraryView libView) {
		super(libView, AuthoringUIResources.reassignAction_text);
	}
	
	protected void doRun() {
		AssignAction assignAction = new AssignAction(getLibraryView());		
		UnassignAction unassignAction = new UnassignAction(getLibraryView());

		assignAction.doRun();
		unassignAction.doRun();
	}
	

}
