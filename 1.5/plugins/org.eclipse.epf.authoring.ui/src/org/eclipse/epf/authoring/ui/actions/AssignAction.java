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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.dialogs.AssignDialog;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.ui.actions.LibraryLockingOperationRunner;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;

/**
 * Assign method element.
 * 
 * @author Weiping Lu
 * @since  1.5
 */
public class AssignAction extends Action {

	/**
	 * Creates an instance
	 * @param text
	 */
	public AssignAction() {
		super(AuthoringUIResources.assignAction_text);
	}
	
	protected void doAssign() {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#run()
	 */
	public void run() {
		LibraryLockingOperationRunner runner = new LibraryLockingOperationRunner();
		runner.run(new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor)
					throws InvocationTargetException,
					InterruptedException {
				doAssign();
			}

		});
	}
	
	/**
	 * @param selection
	 * @return
	 */
	public boolean updateSelection(IStructuredSelection selection) {
		return false;
		//return canAssign(selection);
	}
	
	/**
	 * Returns <code>true</code> if the selected method elements can be assigned.
	 */
	private boolean canAssign(IStructuredSelection selection) {
		if (selection.size() > 1) {
			return false;
		}
		Object element = TngUtil.unwrap(selection.getFirstElement());
		if (element instanceof CustomCategory) {
			return true;
		}
		
		return false;
	}

}
