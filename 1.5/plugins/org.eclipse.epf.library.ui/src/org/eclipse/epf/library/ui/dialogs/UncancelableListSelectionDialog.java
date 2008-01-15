//------------------------------------------------------------------------------
// Copyright (c) 2004, 2005 IBM Corporation.  All Rights Reserved.
//------------------------------------------------------------------------------
package org.eclipse.epf.library.ui.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListSelectionDialog;

/**
 * This dialog extends jface ListSelectionDialog and removes cancel button
  * 
 * @author Phong Nguyen Le
 * @since 1.1
 *
 */
public class UncancelableListSelectionDialog extends ListSelectionDialog {

	public UncancelableListSelectionDialog(Shell parentShell, Object input,
            IStructuredContentProvider contentProvider,
            ILabelProvider labelProvider, String message)
	{
		super(parentShell, input, contentProvider, labelProvider, message);
	}
	
	protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
                true);
       
    }
}
