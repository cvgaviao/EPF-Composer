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

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.authoring.ui.AuthoringUIHelpContexts;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.ConfigurationEditor;
import org.eclipse.epf.authoring.ui.editors.ConfigurationEditorInput;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.edit.LibraryEditResources;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.validation.IValidator;
import org.eclipse.epf.library.edit.validation.IValidatorFactory;
import org.eclipse.epf.library.ui.LibraryUIText;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;


/**
 * The Description page in the Configuration editor.
 * 
 * @author Shashidhar Kannoori
 * @author Jinhua Xi
 * @author Kelvin Low
 * @since 1.0
 */
public class ConfigurationDescription extends FormPage implements IRefreshable {

	private static final String FORM_PREFIX = AuthoringUIResources.ConfigurationDescriptionFormPrefix; 

	private MethodConfiguration config = null;

	private ScrolledForm form = null;

	private Text nameText;
	
	private Text ctrl_presentation_name;

	private Text despText;

	private ModifyListener modelModifyListener;

	private IActionManager actionMgr;

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
		setSite(site);
		setInput(input);
		ConfigurationEditorInput configInput = (ConfigurationEditorInput) input;
		config = configInput.getConfiguration();
	}

	/**
	 * @see org.eclipse.ui.forms.editor.createFormContent(IManagedForm)
	 */
	protected void createFormContent(IManagedForm managedForm) {
		form = managedForm.getForm();
		form.setText(FORM_PREFIX + config.getName());
		FormToolkit toolkit = managedForm.getToolkit();

		TableWrapLayout layout = new TableWrapLayout();
		form.getBody().setLayout(layout);

		Section genSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		createGeneralContent(toolkit, genSection);

		addListeners();
	}

	/**
	 * Create general content for the description page
	 * @param toolkit
	 * @param section
	 */
	public void createGeneralContent(FormToolkit toolkit, Section section) {
		section.setLayoutData(new TableWrapData(TableWrapData.FILL_GRAB));
		section.setText(AuthoringUIResources.ConfigurationDescriptionGeneralInfo); 
		section
				.setDescription(AuthoringUIResources.ConfigurationDescriptionDescription_text); 

		Composite sectionClient = toolkit.createComposite(section);
		TableWrapLayout layout = new TableWrapLayout();
		layout.numColumns = 2;
		sectionClient.setLayout(layout);

		toolkit
				.createLabel(
						sectionClient,
						AuthoringUIResources.ConfigurationDescriptionName, SWT.NONE); 
		nameText = toolkit.createText(sectionClient, "", SWT.NONE); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
		nameText.setLayoutData(td);
		nameText.setText(config.getName());
		
		// presentation name 
		Label l_presentation_name = toolkit.createLabel(sectionClient,
				AuthoringUIText.PRESENTATION_NAME_TEXT, SWT.NONE);

		ctrl_presentation_name = toolkit.createText(sectionClient, "", SWT.NONE); //$NON-NLS-1$
		{
			TableWrapData twd = new TableWrapData(TableWrapData.FILL_GRAB);
			ctrl_presentation_name.setLayoutData(twd);
		}
		ctrl_presentation_name.setText(config.getPresentationName());

		Label despLabel = toolkit
				.createLabel(
						sectionClient,
						AuthoringUIResources.ConfigurationDescriptionDescriptionLabel, SWT.NONE); 
		TableWrapData twd0 = new TableWrapData();
		despLabel.setLayoutData(twd0);

		despText = toolkit.createText(sectionClient,
				"", SWT.NONE | SWT.MULTI | SWT.V_SCROLL | SWT.WRAP); //$NON-NLS-1$
		TableWrapData twd = new TableWrapData(TableWrapData.FILL_GRAB);
		twd.heightHint = 50;
		despText.setLayoutData(twd);
		despText.setText(config.getBriefDescription());

		section.setClient(sectionClient);
		toolkit.paintBordersFor(sectionClient);

		PlatformUI.getWorkbench().getHelpSystem().setHelp(
				sectionClient.getParent(), AuthoringUIHelpContexts.CONFIGURATION_EDITOR_ALL_CONTEXT); 

		Display display = form.getBody().getDisplay();
		if (!(display == null || display.isDisposed())) {
			display.asyncExec(new Runnable() {
				public void run() {
					nameText.setFocus();
					nameText.setSelection(0, nameText.getText().length());
				}
			});
		}
	}

	private void addListeners() {
		final ConfigurationEditor editor = (ConfigurationEditor) getEditor();
		modelModifyListener = editor.createModifyListener(config);

		actionMgr = ((ConfigurationEditor) getEditor()).getActionManager();

		nameText.addModifyListener(modelModifyListener);
		nameText.addFocusListener(new FocusAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusGained(org.eclipse.swt.events.FocusEvent)
			 */
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getNamedElement_Name());
			}
			
			public void focusLost(FocusEvent e) {
				String oldContent = config.getName();
				final MethodConfiguration iConfig = config;
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				if (isTextNonEmpty(nameText)) {
					String name = nameText.getText();
					
					// 178462
					String msg = null;
					if (oldContent.indexOf("&") < 0 && name.indexOf("&") > -1) { //$NON-NLS-1$ //$NON-NLS-2$
						msg = NLS
								.bind(
										LibraryEditResources.invalidElementNameError4_msg,
										name);
					} else {
						IValidator validator = IValidatorFactory.INSTANCE
								.createNameValidator(config.eContainer(),
										config);
						msg = validator.isValid(name);
					}
					
					if (msg != null) {
						AuthoringUIPlugin
								.getDefault()
								.getMsgDialog()
								.displayError(
										AuthoringUIResources.renameDialog_title, 
										msg);
						nameText.getDisplay().asyncExec(new Runnable() {
							public void run() {
								nameText.setText(iConfig.getName());
								nameText.selectAll();
								nameText.setFocus();
							}
						});
						return;
					}
					name = name.trim();			//166755
					
					if (!name.equals(config.getName())) {
						boolean status = actionMgr.doAction(IActionManager.SET,
								config, UmaPackage.eINSTANCE
										.getNamedElement_Name(), name, -1);
						if (status) {
							Resource resource = config.eResource();
							if(resource != null && resource != config.eContainer().eResource()) {
								editor.addResourceToAdjustLocation(config.eResource());
							}
							form.setText(FORM_PREFIX + config.getName());
							nameText.setText(name);
						}
					}
				} else {
					AuthoringUIPlugin
							.getDefault()
							.getMsgDialog()
							.displayError(
									AuthoringUIResources.renameDialog_title, 
									AuthoringUIResources.bind(AuthoringUIResources.emptyElementNameError_msg, StrUtil.toLower(LibraryUIText.TEXT_METHOD_CONFIGURATON))); 
					nameText.setText(config.getName());
					nameText.getDisplay().asyncExec(new Runnable() {
						public void run() {
							nameText.setFocus();
							nameText.setSelection(0, nameText.getText()
									.length());
						}
					});
				}
			}

		});
		
		// add presentation_name listener
		ctrl_presentation_name.addModifyListener(modelModifyListener);
		ctrl_presentation_name.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_PresentationName());
				// when user tab to this field, select all text
				ctrl_presentation_name.selectAll();
			}

			public void focusLost(FocusEvent e) {
				String oldContent = config.getPresentationName();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newName = ctrl_presentation_name.getText();
				if (!newName.equals(config.getPresentationName())) {
					boolean success = actionMgr.doAction(
							IActionManager.SET,
							config,
							UmaPackage.eINSTANCE
									.getMethodElement_PresentationName(),
							newName, -1);
					if (success) {
						ctrl_presentation_name.setText(newName);
					}
				}
				// clear the selection when the focus of the component is lost 
				if(ctrl_presentation_name.getSelectionCount() > 0){
					ctrl_presentation_name.clearSelection();
				} 
			}
		});

		despText.addModifyListener(modelModifyListener);
		despText.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_BriefDescription());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = config.getBriefDescription();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String desc1 = despText.getText();
				if (!desc1.equals(config.getBriefDescription())) {
					actionMgr.doAction(IActionManager.SET, config,
							UmaPackage.eINSTANCE
									.getMethodElement_BriefDescription(),
							desc1, -1);
				}
			}
		});
	}

	/**
	 * Returns <code>true</code> if the given Text control contain characters
	 * other than spaces.
	 * 
	 * @param text
	 *            The Text control.
	 * @return <code>true</code> if the given Text control contain characters
	 *         other than spaces.
	 */
	private static boolean isTextNonEmpty(Text t) {
		String s = t.getText();
		if ((s != null) && (s.trim().length() > 0))
			return true;
		return false;
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#dispose()
	 */
	public void dispose() {
		super.dispose();
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.IRefreshable#refreshName(java.lang.String)
	 */
	public void refreshName(String newName) {
		if (newName != null) {
			if ((nameText != null) && !(nameText.isDisposed())) {
				nameText.removeModifyListener(modelModifyListener);
				nameText.setText(newName);
				nameText.addModifyListener(modelModifyListener);
				form.setText(FORM_PREFIX + config.getName());
			}
		}
	}

}