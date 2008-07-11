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
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Manages the Authoring UI preferences.
 * 
 * @author Kelvin Low
 * @since 1.2
 */
public class AuthoringUIPreferences {

	public static final String ENABLE_LIBRARY_VALIDATION = "enabledLibraryValidation"; //$NON-NLS-1$
	
	public static final String SHOW_LONG_PRESENTATION_NAME = "showLongPresentationName"; //$NON-NLS-1$

	public static final String ADD_TASKS_PER_ROW = "ACTIVITY_DETAIL_DIAGRAM_TASKS_PER_ROW"; //$NON-NLS-1$
	
	private static final int DEFAULT_ADD_TASKS_PER_ROW = 10; 
	
	private static final boolean DEFAULT_ENABLE_LIBRARY_VALIDATION = false;
	
	private static final boolean DEFAULT_SHOW_LONG_PRESENTATION_NAME = false;

	// The plug-in specific preference store.
	private static IPreferenceStore prefStore = AuthoringUIPlugin.getDefault()
			.getPreferenceStore();
	static {
		// Initialize the default preference values.
		prefStore.setDefault(ENABLE_LIBRARY_VALIDATION,
				DEFAULT_ENABLE_LIBRARY_VALIDATION);
		
		prefStore.setDefault(SHOW_LONG_PRESENTATION_NAME,
				DEFAULT_SHOW_LONG_PRESENTATION_NAME);
		
		prefStore.setDefault(ADD_TASKS_PER_ROW, DEFAULT_ADD_TASKS_PER_ROW);
		
	}

	/**
	 * Gets the default enable method library validation preference.
	 */
	public static boolean getDefaultEnableLibraryValidation() {
		return DEFAULT_ENABLE_LIBRARY_VALIDATION;
	}

	/**
	 * Gets the method library validation preference.
	 */
	public static boolean getEnableLibraryValidation() {
		return prefStore.getBoolean(ENABLE_LIBRARY_VALIDATION);
	}

	/**
	 * Sets the method library validation preference.
	 */
	public static void setEnableLibraryValidation(boolean value) {
		prefStore.setValue(ENABLE_LIBRARY_VALIDATION, value);
	}
	
	/**
	 * Gets the default show long presentation name preference.
	 */
	public static boolean getDefaultShowLongPresentationName() {
		return DEFAULT_SHOW_LONG_PRESENTATION_NAME;
	}
	
	/**
	 * Gets the
	 */
	public static boolean getShowLongPresentationName() {
		return prefStore.getBoolean(SHOW_LONG_PRESENTATION_NAME);
	}

	/**
	 * Sets the show long presentation name preference.
	 */
	public static void setShowLongPresentationName(boolean value) {
		prefStore.setValue(SHOW_LONG_PRESENTATION_NAME, value);
	}
	
	/**
	 * Gets the ADD_TASKS_PER_ROW preference.
	 */
	public static int getADD_TasksPerRow()  {
		int value = prefStore.getInt(ADD_TASKS_PER_ROW);
		return value > 0 ? value : DEFAULT_ADD_TASKS_PER_ROW;
	}

	/**
	 * Sets the ADD_TASKS_PER_ROW preference.
	 */
	public static void setADDTasksPerRow(int value) {
		prefStore.setValue(ADD_TASKS_PER_ROW, value);
	}
	
	/**
	 * Gets the default ADD_TASKS_PER_ROW preference.
	 */
	public static int getDefaultADDTasksPerRow() {
		return DEFAULT_ADD_TASKS_PER_ROW;
	}

}
