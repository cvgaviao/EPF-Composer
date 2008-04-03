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
package org.eclipse.epf.authoring.ui.forms;

import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.editors.ConfigurationEditorInput;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationProperties;
import org.eclipse.epf.library.edit.command.MethodElementSetPropertyCommand;
import org.eclipse.epf.library.edit.util.MethodElementPropertyHelper;
import org.eclipse.epf.ui.util.SWTUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;


/**
 * The Description page in the Configuration editor.
 * 
 * @author Shashidhar Kannoori
 * @author Jinhua Xi
 * @author Kelvin Low
 * @since 1.0
 */
public class ConfigurationDescription extends DescriptionFormPage implements IRefreshable {

	private static final String FORM_PREFIX = AuthoringUIResources.ConfigurationDescriptionFormPrefix; 

	private MethodConfiguration config = null;
	
	private ConfigurationProperties configProperties;
	private Button hideErrorButton;
	private Button hideWarnButton;
	private Button hideInfoButton;

	/**
	 * Creates a new instance.
	 */
	public ConfigurationDescription(FormEditor editor) {
		super(
				editor,
				AuthoringUIResources.ConfigurationDescriptionDescription, AuthoringUIResources.ConfigurationDescriptionDescription); 
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		ConfigurationEditorInput configInput = (ConfigurationEditorInput) input;
		config = configInput.getConfiguration();
		versionSectionOn = false;
		detailSectionOn = false;
		variabilitySectionOn = false;
		fullDescOn = false;
		keyConsiderationOn = false;
		
		configProperties = LibraryService.getInstance()
				.getConfigurationManager(config).getConfigurationProperties();
	}

	@Override
	protected void createEditorContent(FormToolkit toolkit) {
		super.createEditorContent(toolkit);
		// Set focus on the Name text control.
		Display display = form.getBody().getDisplay();
		if (!(display == null || display.isDisposed())) {
			display.asyncExec(new Runnable() {
				public void run() {
					if(ctrl_name.isDisposed()) return;		
					ctrl_name.setFocus();
					ctrl_name.setSelection(0, ctrl_name.getText().length());
				}
			});
		}
	}

	@Override
	public void loadSectionDescription() {
		this.generalSectionDescription = AuthoringUIResources.ConfigurationDescriptionDescription_text;
	}

	@Override
	protected Object getContentElement() {
		return config;
	}
	
	@Override
	protected void addListeners() {
		super.addListeners();
		
		hideErrorButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				Object obj = e.getSource();
				if (obj instanceof Button) {
					Button button = (Button) obj;
					String value = button.getSelection() ? Boolean.TRUE
							.toString() : Boolean.FALSE.toString();
					actionMgr
							.execute(new ConfigSetPropertyCommand(
									config,
									MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_ERRORS,
									value));
				}
			}
		});
		
		hideWarnButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				Object obj = e.getSource();
				if (obj instanceof Button) {
					Button button = (Button) obj;
					String value = button.getSelection() ? Boolean.TRUE
							.toString() : Boolean.FALSE.toString();
					actionMgr
							.execute(new ConfigSetPropertyCommand(
									config,
									MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_WARNINGS,
									value));
				}
			}
		});
		
		hideInfoButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				Object obj = e.getSource();
				if (obj instanceof Button) {
					Button button = (Button) obj;
					String value = button.getSelection() ? Boolean.TRUE
							.toString() : Boolean.FALSE.toString();
					actionMgr
							.execute(new ConfigSetPropertyCommand(
									config,
									MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_INFOS,
									value));
				}
			}
		});
		
	}
	
	@Override
	protected void createGeneralSection(FormToolkit toolkit) {
		super.createGeneralSection(toolkit);

		Group group = new Group(generalComposite, SWT.NONE);
		group.setLayout(new GridLayout(1, false));
		GridData data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
		data.horizontalSpan = 4;
		group.setLayoutData(data);
		group.setText(AuthoringUIResources.configProblemViewOptionsText);

		hideErrorButton = SWTUtil.createCheckbox(group, AuthoringUIResources.hideErrosText);
		hideWarnButton = SWTUtil.createCheckbox(group, AuthoringUIResources.hideWarningsText);
		hideInfoButton = SWTUtil.createCheckbox(group,AuthoringUIResources.hideInfosText);
		
		hideErrorButton.setSelection(configProperties.isHideErrors());
		hideWarnButton.setSelection(configProperties.isHideWarnings());
		hideInfoButton.setSelection(configProperties.isHideInfos());		
	}
	
	class ConfigSetPropertyCommand extends MethodElementSetPropertyCommand {		
		public ConfigSetPropertyCommand(MethodElement element, String key, String value) {
			super(element, key, value);
		}
		
		@Override
		public void redo() {
			super.redo();
			if (key.equals(MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_ERRORS)) {
				configProperties.setHideErrors(value);
			} else if (key.equals(MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_WARNINGS)) {
				configProperties.setHideWarnings(value);
			} else if (key.equals(MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_INFOS)) {
				configProperties.setHideInfos(value);
			}
		}
		
		@Override
		public void undo() {
			super.undo();
			if (key.equals(MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_ERRORS)) {
				configProperties.setHideErrors(oldValue);
			} else if (key.equals(MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_WARNINGS)) {
				configProperties.setHideWarnings(oldValue);
			} else if (key.equals(MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_INFOS)) {
				configProperties.setHideInfos(oldValue);
			}
		}
	}

}