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
package org.eclipse.epf.publishing.ui.preferences;

import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.library.ui.LibraryUIPlugin;
import org.eclipse.epf.library.ui.preferences.LibraryUIPreferences;
import org.eclipse.epf.publishing.ui.PublishingUIPlugin;
import org.eclipse.epf.ui.preferences.BasePreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


/**
 * Preference page for diagram options
 * 
 * @author Phong Le
 * @author Shilpa Toraskar
 * @since 1.0
 * 
 */
public class ActivityDiagramPreferencePage extends BasePreferencePage implements
		IWorkbenchPreferencePage, SelectionListener {

	Composite composite;

	int NUM_COLUMN = 3;

	private Button ctrl_publish_unopen_activitydd;

	private Button ctrl_publish_ad_for_activity_extension;
	
	public ActivityDiagramPreferencePage() {
		super();
	}

	protected Control createContents(Composite parent) {

		composite = new Composite(parent, SWT.NULL);
		composite.setLayout(new GridLayout(1, false));

		// Create activity diagram group.
		Group activityDiagramGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.preference_Activity_Diagram, 1);

		ctrl_publish_ad_for_activity_extension = createCheckbox(
				activityDiagramGroup,
				AuthoringUIResources.prompt_for_publish_extend_activity_diagram);

		// Create activity detatil diagram group.
		Group activityDetailDiagramGroup = createGridLayoutGroup(composite,
				AuthoringUIResources.preference_Activity_Detail_Diagram, 1);

		ctrl_publish_unopen_activitydd = createCheckbox(
				activityDetailDiagramGroup,
				AuthoringUIResources.promptfor_publish_unopen_activitydd_text);
		
//		ctrl_publish_unopen_activitydd = new Button(composite, SWT.CHECK);
//		ctrl_publish_unopen_activitydd
//				.setText(AuthoringUIResources.promptfor_publish_unopen_activitydd_text); 
//
//		GridData data = new GridData();
//		data.horizontalSpan = 3;
//		data.horizontalAlignment = GridData.FILL;
//		ctrl_publish_unopen_activitydd.setLayoutData(data);
//		
//		ctrl_publish_ad_for_activity_extension = new Button(composite, SWT.CHECK);
//		ctrl_publish_ad_for_activity_extension
//				.setText(AuthoringUIResources.prompt_for_publish_extend_activity_diagram); 
//				
		
		initializeValues();

		return composite;
	}

	public void init(IWorkbench workbench) {

	}

	public void widgetSelected(SelectionEvent e) {

	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	/*
	 * (non-Javadoc) Method declared on PreferencePage
	 */
	protected void performDefaults() {
		super.performDefaults();
		initializeDefaults();
	}

	/*
	 * (non-Javadoc) Method declared on PreferencePage
	 */
	public boolean performOk() {
		storeValues();
		LibraryUIPlugin.getDefault().savePluginPreferences();

		// update the settings for browsing
		PublishingUIPlugin.getDefault().updateLayoutSettings();

		return true;
	}

	/**
	 * Stores the values of the controls back to the preference store.
	 */
	private void storeValues() {
		// IPreferenceStore store = getPreferenceStore();
		// store.setValue(LibraryPreferenceConstants.PREF_PROMPT_FOR_LIBRARY_AT_STARTUP,
		// ctrl_publish_unopen_activitydd.getSelection());
		LibraryUIPreferences
				.setPublishUnopenActivitydd(ctrl_publish_unopen_activitydd
						.getSelection());
		LibraryUIPreferences
				.setPublishADForActivityExtension(ctrl_publish_ad_for_activity_extension
						.getSelection());
	}

	private void initializeDefaults() {
		// IPreferenceStore store = getPreferenceStore();
		ctrl_publish_unopen_activitydd.setSelection(false);
		ctrl_publish_ad_for_activity_extension.setSelection(true);
	}

	/**
	 * Initializes states of the controls from the preference store.
	 */
	private void initializeValues() {
		// IPreferenceStore store = getPreferenceStore();
		// ctrl_publish_unopen_activitydd.setSelection(store.getBoolean(LibraryPreferenceConstants.PREF_PROMPT_FOR_LIBRARY_AT_STARTUP));
		ctrl_publish_unopen_activitydd.setSelection(LibraryUIPreferences
				.getPublishUnopenActivitydd());
		ctrl_publish_ad_for_activity_extension
				.setSelection(LibraryUIPreferences
						.getPublishADForActivityExtension());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.epf.authoring.ui.preferences.CommonPrefPage#doGetPreferenceStore()
	 */
	protected IPreferenceStore doGetPreferenceStore() {
		return LibraryUIPlugin.getDefault().getPreferenceStore();
	}

}
