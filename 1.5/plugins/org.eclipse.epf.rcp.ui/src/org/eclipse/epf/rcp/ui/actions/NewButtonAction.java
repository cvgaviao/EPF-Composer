//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.rcp.ui.actions;

import org.eclipse.epf.library.ui.LibraryUIText;
import org.eclipse.epf.rcp.ui.RCPUIPlugin;
import org.eclipse.epf.rcp.ui.RCPUIResources;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * Displays a drop down combo for creating new method library, plug-in and
 * configuration. This action is invoked via the New button on the system
 * toolbar.
 * 
 * @author Bingxue Xu
 * @author Kelvin Low
 * @since 1.0
 */
public class NewButtonAction extends Action implements IMenuCreator {

	private Menu fileNewDropdown = null;

	/* Shell-Sharing
	private NewLibraryAction newLibraryAction = null;

	private NewPluginAction newPluginAction = null;

	private NewConfigurationAction newConfigurationAction = null;

	private NewWizardAction newWizardAction = null;
	*/

	/**
	 * Creates a new instance.
	 */
	public NewButtonAction() {
		setId("fileNewDropdown"); //$NON-NLS-1$
		setToolTipText(RCPUIResources.newDropDown_tooltip_text);

		ImageDescriptor imageDescriptor = RCPUIPlugin.getDefault()
				.getImageDescriptor("new_con.gif"); //$NON-NLS-1$
		setImageDescriptor(imageDescriptor);

		setMenuCreator(this);
	}

	/**
	 * @see org.eclipse.jface.action.IMenuCreator#getMenu(Control)
	 */
	public Menu getMenu(Control parent) {
		if (fileNewDropdown != null) {
			return fileNewDropdown;
		}

		fileNewDropdown = new Menu(parent);
		MenuItem newLibraryMItem = new MenuItem(fileNewDropdown, SWT.PUSH);
		newLibraryMItem.setText(LibraryUIText.TEXT_METHOD_LIBARARY);
		new MenuItem(fileNewDropdown, SWT.SEPARATOR);
		MenuItem newPluginMItem = new MenuItem(fileNewDropdown, SWT.PUSH);
		newPluginMItem.setText(LibraryUIText.TEXT_METHOD_PLUGIN);
		MenuItem newConfigMItem = new MenuItem(fileNewDropdown, SWT.PUSH);
		newConfigMItem.setText(LibraryUIText.TEXT_METHOD_CONFIGURATON);
		new MenuItem(fileNewDropdown, SWT.SEPARATOR);
		MenuItem newOtherMItem = new MenuItem(fileNewDropdown, SWT.PUSH);
		newOtherMItem.setText(RCPUIResources.otherMenuItem_text);

		/* Shell-Sharing		
		newLibraryAction = new NewLibraryAction();
		newPluginAction = new NewPluginAction();
		newConfigurationAction = new NewConfigurationAction();
		if (newWizardAction == null) {
			IWorkbenchWindow wbWindow = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow();
			newWizardAction = new NewWizardAction(wbWindow);
		}

		newLibraryMItem.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event e) {
				newLibraryAction.run();
			}
		});
		
		newPluginMItem.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event e) {
				newPluginAction.run();
			}
		});
		newConfigMItem.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event e) {
				newConfigurationAction.run();
			}
		});
		
		newOtherMItem.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event e) {
				newWizardAction.run();
			}
		});
		*/

		return fileNewDropdown;
	}

	/**
	 * @see org.eclipse.jface.action.IMenuCreator#getMenu(Menu)
	 */
	public Menu getMenu(Menu parent) {
		return null;
	}

	/**
	 * @see org.eclipse.jface.action.Action#run()
	 */
	public void run() {
		/*
		if (newWizardAction == null) {
			IWorkbenchWindow wbWindow = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow();
			newWizardAction = new NewWizardAction(wbWindow);
		}
		newWizardAction.run();
		*/
	}

	/**
	 * dispose the fileNewDropdown object
	 */
	public void dispose() {
		if (fileNewDropdown != null) {
			fileNewDropdown.dispose();
			fileNewDropdown = null;
		}
	}

}