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
package org.eclipse.epf.authoring.ui.dialogs;

import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.ui.util.SWTUtil;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


/**
 * Dialog for showing details in Problem View
 * 
 * @author Weiping Lu
 * @since 1.5
 */
public class ShowDetailsProblemViewDialog extends Dialog {

	private Text details;
	private String detailsStr;
	
	public ShowDetailsProblemViewDialog(Shell parentShell, String details) {
		super(parentShell);
		detailsStr = details;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);

		GridLayout layout = (GridLayout) composite.getLayout();
		layout.marginWidth = 10;
		layout.marginHeight = 10;

		Label label = new Label(composite, SWT.NONE);
		label.setText("Detail Description"); 
		GridData layoutData = new GridData(SWT.BEGINNING);
		label.setLayoutData(layoutData);

		GridData spec = new GridData(GridData.FILL_BOTH);
		
		details = SWTUtil.createEditableText(composite, 400, 80, 2);
		details.setText(detailsStr);
		
		return composite;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Show Details"); 
	}	

	public Text getDetails() {
		return details;
	}
	

}
