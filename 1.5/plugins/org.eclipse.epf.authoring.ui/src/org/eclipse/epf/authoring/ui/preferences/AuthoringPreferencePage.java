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
package org.eclipse.epf.authoring.ui.preferences;

import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.EditorChooser;
import org.eclipse.epf.common.ui.util.CommonPreferences;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.preferences.LibraryPreferences;
import org.eclipse.epf.library.ui.LibraryUIPlugin;
import org.eclipse.epf.library.ui.preferences.LibraryUIPreferences;
import org.eclipse.epf.ui.preferences.BasePreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * The Authoring preference page.
 * 
 * @author Kelvin Low
 * @author Shilpa Toraskar
 * @since 1.2
 */
public class AuthoringPreferencePage extends BasePreferencePage implements
		ModifyListener {

	private static int MIN_PREFERENCE_HISTORY_SIZE = 1;

	private static int MAX_PREFERENCE_HISTORY_SIZE = 20;

	private Composite composite;

	private Text defaultLibraryPathText;

	private Button browseButton;

	private Button discardUnresolvedReferencesCheckbox;

	private Button useNewExtendsSemanticsCheckbox;

	private Button enableLibraryValidationCheckbox;

	private Text preferenceHistorySizeText;
	
	private Button enableUIFieldsCheckbox;

	/**
	 * Creates and returns the SWT control for the customized body of this
	 * preference page under the given parent composite.
	 * 
	 * @param parent
	 *            the parent composite
	 * @return the new control
	 */
	protected Control createContents(Composite parent) {
		composite = createGridLayoutComposite(parent, 1);

		// Create the Method library group.
		Group libraryGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.methodLibraryGroup_text, 3);

		createLabel(libraryGroup, AuthoringUIResources.defaultPathLabel_text);
		defaultLibraryPathText = createEditableText(libraryGroup);
		browseButton = createButton(libraryGroup,
				AuthoringUIText.BROWSE_BUTTON_TEXT);

		discardUnresolvedReferencesCheckbox = createCheckbox(libraryGroup,
				AuthoringUIResources.discardunresolvedref, 3);

		// Create the Modeling group.
		Group modelingGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.modelingGroup_text, 1);

		useNewExtendsSemanticsCheckbox = createCheckbox(modelingGroup,
				AuthoringUIResources.extend_semantics_button_text);

		// Create the UI group.
		Group userInterfaceGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.userInterfaceGroup_text, 2);

		createLabel(userInterfaceGroup,
				AuthoringUIResources.default_list_length_label);
		preferenceHistorySizeText = createEditableText(userInterfaceGroup,
				"", 25); //$NON-NLS-1$

		// Create the debug group.
		Group debugGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.debugGroup_text, 2);

		enableLibraryValidationCheckbox = createCheckbox(debugGroup,
				AuthoringUIResources.enableLibraryValidationCheckbox_text);
		
		// Create the editor group.
		Group editorGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.editorGroup_text, 2);

		enableUIFieldsCheckbox = createCheckbox(editorGroup,
				AuthoringUIResources.enableUIFieldsCheckbox_text);

		initControls();

		addListeners();

		return composite;
	}

	/**
	 * Initializes the preference page controls with data.
	 */
	protected void initControls() {
		defaultLibraryPathText.setText(LibraryUIPreferences
				.getDefaultLibraryPath());

		discardUnresolvedReferencesCheckbox.setSelection(LibraryPreferences
				.getDiscardUnresolvedReferences());

		useNewExtendsSemanticsCheckbox.setSelection(LibraryPreferences
				.getUseNewExtendsSemantics());

		preferenceHistorySizeText.setText("" //$NON-NLS-1$
				+ CommonPreferences.getPreferenceHistorySize()); 

		enableLibraryValidationCheckbox.setSelection(AuthoringUIPreferences
				.getEnableLibraryValidation());
		
		enableUIFieldsCheckbox.setSelection(AuthoringUIPreferences
				.getEnableUIFields());
	}

	/**
	 * Adds event listeners to the preference page controls.
	 */
	protected void addListeners() {
		browseButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				openDirectoryDialog();
			}
		});

		preferenceHistorySizeText.addModifyListener(this);
		
		enableUIFieldsCheckbox.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				Shell shell = Display.getCurrent().getActiveShell();
				if (AuthoringUIPlugin.getDefault().getMsgDialog()
						.displayConfirmation(shell.getText(), AuthoringUIResources.enableUIFieldsChange_message)) {
					// close editors with saving
					EditorChooser.getInstance().closeAllMethodEditorsWithSaving();
					
				} else {
					enableUIFieldsCheckbox.setSelection(!enableUIFieldsCheckbox
							.getSelection());
				}
			}
		});
	}

	/**
	 * @see org.eclipse.swt.events.ModifyListener#modifyText(ModifyEvent)
	 */
	public void modifyText(ModifyEvent e) {
		setErrorMessage(null);
		setValid(true);

		int value = 0;
		if (e.widget == preferenceHistorySizeText) {
			value = getPreferenceHistorySize();
			if (value < MIN_PREFERENCE_HISTORY_SIZE
					|| value > MAX_PREFERENCE_HISTORY_SIZE) {
				setErrorMessage(AuthoringUIResources
						.bind(
								AuthoringUIResources.invalidPreferenceHistorySizeError_msg,
								new Object[] {
										new Integer(MIN_PREFERENCE_HISTORY_SIZE),
										new Integer(MAX_PREFERENCE_HISTORY_SIZE) }));
				setValid(false);
			}
		}
	}

	/**
	 * Performs special processing when this page's Defaults button has been
	 * pressed.
	 */
	protected void performDefaults() {
		super.performDefaults();

		String defaultLibraryPath = LibraryUIPreferences
				.getInitialDefaultLibraryPath();
		LibraryUIPreferences.setDefaultLibraryPath(defaultLibraryPath);
		defaultLibraryPathText.setText(defaultLibraryPath);

		boolean discardUnresolvedReferences = LibraryPreferences
				.getDefaultDiscardUnresolvedReferences();
		LibraryPreferences
				.setDiscardUnresolvedReferences(discardUnresolvedReferences);
		discardUnresolvedReferencesCheckbox
				.setSelection(discardUnresolvedReferences);

		boolean useNewExtendsSemantics = LibraryPreferences
				.getDefaultUseNewExtendsSemantics();
		LibraryPreferences.setUseNewExtendsSemantics(useNewExtendsSemantics);
		useNewExtendsSemanticsCheckbox.setSelection(useNewExtendsSemantics);

		int preferenceHistorySize = CommonPreferences
				.getDefaultPreferenceHistorySize();
		CommonPreferences.setPreferenceHistorySize(preferenceHistorySize);
		preferenceHistorySizeText.setText("" + preferenceHistorySize); //$NON-NLS-1$

		boolean enableLibraryValidation = AuthoringUIPreferences
				.getDefaultEnableLibraryValidation();
		AuthoringUIPreferences
				.setEnableLibraryValidation(enableLibraryValidation);
		enableLibraryValidationCheckbox.setSelection(enableLibraryValidation);
		
		boolean enableUIFields = AuthoringUIPreferences
				.getDefaultEnableUIFields();
		AuthoringUIPreferences
				.setEnableUIFields(enableUIFields);
		enableUIFieldsCheckbox.setSelection(enableUIFields);
	}

	/**
	 * @see org.eclipse.jface.preference.PreferencePage#performOk()
	 */
	public boolean performOk() {
		LibraryUIPreferences.setDefaultLibraryPath(getDefaultLibraryPath());
		LibraryPreferences
				.setDiscardUnresolvedReferences(getDiscardUnresolvedReferences());
		LibraryPreferences
				.setUseNewExtendsSemantics(getUseNewExtendsSemantics());
		CommonPreferences.setPreferenceHistorySize(getPreferenceHistorySize());
		AuthoringUIPreferences
			.setEnableLibraryValidation(getEnableLibraryValidation());
		AuthoringUIPreferences
				.setEnableUIFields(getEnableUIFields());
		
		// update the settings for browsing
		LibraryUIPlugin.getDefault().updateLayoutSettings();

		return true;
	}

	/**
	 * Gets the user specified default method library path.
	 */
	protected String getDefaultLibraryPath() {
		return defaultLibraryPathText.getText().trim();
	}

	/**
	 * Gets the user specified discard unresolved references preference.
	 */
	protected boolean getDiscardUnresolvedReferences() {
		return discardUnresolvedReferencesCheckbox.getSelection();
	}

	/**
	 * Gets the user specified use new extends semantics preference.
	 */
	protected boolean getUseNewExtendsSemantics() {
		return useNewExtendsSemanticsCheckbox.getSelection();
	}

	/**
	 * Gets the user specified preference history size.
	 */
	protected int getPreferenceHistorySize() {
		return StrUtil.getIntValue(preferenceHistorySizeText.getText().trim(),
				0);
	}
	
	/**
	 * Gets the enabled library validation preference
	 * @return
	 */
	protected boolean getEnableLibraryValidation() {
		return enableLibraryValidationCheckbox.getSelection();
	}
	
	/**
	 * Gets the show long presentation name / external ID preference
	 * @return
	 */
	protected boolean getEnableUIFields() {
		return enableUIFieldsCheckbox.getSelection();
	}

	/**
	 * Opens the directory dialog.
	 */
	private void openDirectoryDialog() {
		try {
			DirectoryDialog dd = new DirectoryDialog(composite.getShell(),
					SWT.NONE);
			String path = dd.open();
			if (path != null) {
				defaultLibraryPathText.setText(path);
			}
		} catch (Exception e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
		}
	}

}
