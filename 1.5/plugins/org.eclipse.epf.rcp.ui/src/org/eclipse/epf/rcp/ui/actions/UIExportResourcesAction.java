//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/eplv10.html
//
// Contributors:
// IBM Corporation  initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.rcp.ui.actions;

import org.eclipse.epf.rcp.ui.wizards.UIExportWizard;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ExportResourcesAction;
import org.eclipse.ui.internal.IWorkbenchHelpContextIds;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.internal.WorkbenchPlugin;

/**
 * 
 * @author Bingxue Xu
 * @since 1.0
 */

public class UIExportResourcesAction extends ExportResourcesAction {

	private static final int SIZING_WIZARD_WIDTH = 470;

	private static final int SIZING_WIZARD_HEIGHT = 550;

	/**
	 * The workbench window; or <code>null</code> if this action has been
	 * <code>dispose</code>d.
	 */
	private IWorkbenchWindow workbenchWindow;

	/**
	 * Constructor
	 * @param workbench
	 */
	public UIExportResourcesAction(IWorkbench workbench) {
		this(workbench.getActiveWorkbenchWindow());
	}

	/**
	 * Constructor
	 * @param workbench
	 * @param label
	 */
	public UIExportResourcesAction(IWorkbench workbench, String label) {
		this(workbench.getActiveWorkbenchWindow(), label);
	}

	/**
	 * Constructor
	 * @param window
	 */
	public UIExportResourcesAction(IWorkbenchWindow window) {
		this(window, WorkbenchMessages.ExportResourcesAction_text);
	}

	/**
	 * Constructor
	 * @param window
	 * @param label
	 */
	public UIExportResourcesAction(IWorkbenchWindow window, String label) {
		super(window, label);
		this.workbenchWindow = window;
	}

	public UIExportWizard getUIExportWizard() {
		return new UIExportWizard();
	}
	
	/**
	 * Invoke the Export wizards selection Wizard.
	 */
	public void run() {
		if (workbenchWindow == null) {
			// action has been disposed
			return;
		}
		UIExportWizard wizard = getUIExportWizard();
		IStructuredSelection selectionToPass;
		// get the current workbench selection
		ISelection workbenchSelection = workbenchWindow.getSelectionService()
				.getSelection();
		if (workbenchSelection instanceof IStructuredSelection) {
			selectionToPass = (IStructuredSelection) workbenchSelection;
		} else {
			selectionToPass = StructuredSelection.EMPTY;
		}

		wizard.init(workbenchWindow.getWorkbench(), selectionToPass);
		IDialogSettings workbenchSettings = WorkbenchPlugin.getDefault()
				.getDialogSettings();
		IDialogSettings wizardSettings = workbenchSettings
				.getSection("ExportResourcesAction");  //$NON-NLS-1$
		if (wizardSettings == null)
			wizardSettings = workbenchSettings
					.addNewSection("ExportResourcesAction");  //$NON-NLS-1$
		wizard.setDialogSettings(wizardSettings);
		wizard.setForcePreviousAndNextButtons(true);

		Shell parent = workbenchWindow.getShell();
		WizardDialog dialog = new WizardDialog(parent, wizard);
		dialog.create();
		dialog.getShell().setSize(
				Math.max(SIZING_WIZARD_WIDTH, dialog.getShell().getSize().x),
				SIZING_WIZARD_HEIGHT);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(dialog.getShell(),
				IWorkbenchHelpContextIds.EXPORT_WIZARD);
		dialog.open();
	}

}