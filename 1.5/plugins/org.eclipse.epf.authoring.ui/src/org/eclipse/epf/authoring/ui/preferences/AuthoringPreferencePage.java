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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.EditorChooser;
import org.eclipse.epf.authoring.ui.util.AuthoringAccessibleListener;
import org.eclipse.epf.common.ui.util.CommonPreferences;
import org.eclipse.epf.common.ui.util.MsgBox;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.preferences.LibraryPreferences;
import org.eclipse.epf.library.ui.LibraryUIPlugin;
import org.eclipse.epf.library.ui.preferences.LibraryUIPreferences;
import org.eclipse.epf.ui.preferences.BasePreferencePage;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

/**
 * The Authoring preference page.
 * 
 * @author Kelvin Low
 * @author Shilpa Toraskar
 * @since 1.2
 */
public class AuthoringPreferencePage extends BasePreferencePage implements
		ModifyListener {

	private static int MIN_PREFERENCE_HISTORY_SIZE = 5;

	private static int MAX_PREFERENCE_HISTORY_SIZE = 20;

	private Composite composite;

	private Text defaultLibraryPathText;

	private Button browseButton;

	private Button discardUnresolvedReferencesCheckbox;

	private Button useNewExtendsSemanticsCheckbox;

	private Button enableLibraryValidationCheckbox;

	private Text preferenceHistorySizeText;
	
	private Button enableUIFieldsCheckbox;
	
	private Button enableAutoNameGenCheckbox;
	
	private Button skipAllCheckbox;
	
	private Button skipSelectedChebox;
	
	private TableViewer selectedHexByteViewer;
	
	private Set<String> selectedHexByteStrSet;
	
	private Button addButton;
	private Button removeButton;
	
	private static Set<String> skipableSet = new HashSet<String>();
	
	static {
		String str = 
			"24,26,2B,2C,2F,3A,3B,3D,3F,40,20,22,3C,3E,23,25,7B,7D,7C,5C,5E,7E,5B,5D,60"; //$NON-NLS-1$

		List<String> list = TngUtil.convertStringsToList(str);
		for (String s : list) {
			skipableSet.add("%" + s);		//$NON-NLS-1$
		}		
	}

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
				AuthoringUIResources.editorGroup_text, 1);

		enableUIFieldsCheckbox = createCheckbox(editorGroup,
				AuthoringUIResources.enableUIFieldsCheckbox_text);
		
		enableAutoNameGenCheckbox = createCheckbox(editorGroup,
				AuthoringUIResources.enableAutoNameGenCheckbox_text);		
		
		// Create the editor group.
		Group rteGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.rteGroup_text, 2);

		skipAllCheckbox = createCheckbox(rteGroup,
				AuthoringUIResources.skipAllCheckbox_text, 2);
		
		skipSelectedChebox = createCheckbox(rteGroup,
				AuthoringUIResources.skipSelectedChebox_text, 2);
		
		selectedHexByteViewer =  new TableViewer(rteGroup);
		GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
		gridData.heightHint = 100;
		selectedHexByteViewer.getTable().setLayoutData(gridData);
		selectedHexByteViewer.setContentProvider(new ArrayContentProvider() {
		    public Object[] getElements(Object inputElement) {
		        if (inputElement instanceof Collection) {		        	
		        	List list = new ArrayList((Collection) inputElement);
		        	Collections.sort(list);
					return list.toArray();
				}
		        return super.getElements(inputElement);
		    }
		});
		selectedHexByteViewer.setLabelProvider(new LabelProvider());
		
		getSelectedHexByteStrSet().add("%20");		
		selectedHexByteViewer.setInput(getSelectedHexByteStrSet());
		
		createAddRemoveButtons(rteGroup);
		
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
		
		enableAutoNameGenCheckbox.setSelection(AuthoringUIPreferences
				.getEnableAutoNameGen());
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
		
		boolean enableAutoNameGen = AuthoringUIPreferences
				.getDefaultEnableAutoNameGen();
		AuthoringUIPreferences
			.setEnableAutoNameGen(enableAutoNameGen);
		enableAutoNameGenCheckbox.setSelection(enableAutoNameGen);
		
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
		AuthoringUIPreferences
				.setEnableAutoNameGen(getEnableAutoNameGen());
		
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
	 * Gets auto name gen flag
	 * @return
	 */
	protected boolean getEnableAutoNameGen() {
		return enableAutoNameGenCheckbox.getSelection();
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
	
	protected void createAddRemoveButtons(Composite parent) {
		Composite buttonComposite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		buttonComposite.setLayout(layout);
		GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
		buttonComposite.setLayoutData(data);

		addButton = new Button(buttonComposite, SWT.PUSH);
		data = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
		addButton.setLayoutData(data);
		addButton
				.setText(AuthoringUIText.ADD_BUTTON_TEXT);
		addButton.getAccessible().addAccessibleListener(
				new AuthoringAccessibleListener(
						AuthoringUIText.ADD_BUTTON_TEXT));
		addButton.addSelectionListener(newAddButtonListener());
		
		removeButton = new Button(buttonComposite, SWT.PUSH);
		data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
		removeButton.setLayoutData(data);
		removeButton
				.setText(AuthoringUIText.REMOVE_BUTTON_TEXT);

		removeButton.getAccessible().addAccessibleListener(
				new AuthoringAccessibleListener(
						AuthoringUIText.REMOVE_BUTTON_TEXT));
		removeButton
				.addSelectionListener(newRemoveButtonListener());
	}
	
	private SelectionListener newAddButtonListener() {
		return new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				ElementListSelectionDialog dialog = new ElementListSelectionDialog(MsgBox.getDefaultShell(), 
						new LabelProvider());
				
				List<String> selectableList = new ArrayList<String>();
				for (String str : skipableSet) {
					if (! getSelectedHexByteStrSet().contains(str)) {
						selectableList.add(str);	
					}
				}
				Collections.sort(selectableList);
				
				dialog.setElements(selectableList.toArray());
				dialog.setMultipleSelection(true);
				dialog.setMessage(AuthoringUIResources.rteGroupAddDialog_msg);
				dialog.setTitle(AuthoringUIResources.rteGroupAddDialog_title);
				if (dialog.open() == Dialog.CANCEL) {
					return;
				}
				
				Object[] objs = dialog.getResult();
				if (objs == null || objs.length == 0) {
					return;
				}
				
				for (Object obj : objs) {
					if (obj instanceof String)
					getSelectedHexByteStrSet().add((String) obj);
				}
				
				selectedHexByteViewer.refresh();
			}
		};
	}
	
	private SelectionListener newRemoveButtonListener() {
		return new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				ISelection s = selectedHexByteViewer.getSelection();
				if (s instanceof IStructuredSelection) {					
					for (Iterator it = ((IStructuredSelection) s).iterator(); it.hasNext();) {
						getSelectedHexByteStrSet().remove(it.next());
					}
				}
				selectedHexByteViewer.refresh();
			}
		};
	}

	private Set<String> getSelectedHexByteStrSet() {
		if (selectedHexByteStrSet == null) {
			selectedHexByteStrSet = new HashSet<String>();
		}
		return selectedHexByteStrSet;
	}
	

}
