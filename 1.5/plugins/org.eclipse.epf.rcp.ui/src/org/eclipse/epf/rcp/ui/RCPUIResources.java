/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.epf.rcp.ui;

import org.eclipse.osgi.util.NLS;

public final class RCPUIResources extends NLS {

	private static String BUNDLE_NAME = RCPUIResources.class.getPackage()
			.getName()
			+ ".Resources"; //$NON-NLS-1$

	private RCPUIResources() {
		// Do not instantiate
	}

	public static String fileMenuItem_text;

	public static String fileNewMenuItem_text;

	public static String fileOpenMenuItem_text;

	public static String editMenuItem_text;

	public static String navigateMenuItem_text;

	public static String searchMenuItem_text;

	public static String configurationMenuItem_text;

	public static String windowMenuItem_text;

	public static String windowOpenPerspectiveMenuItem_text;

	public static String windowShowViewMenuItem_text;

	public static String helpMenuItem_text;

	public static String newDropDown_tooltip_text;

	public static String otherMenuItem_text;

	public static String openAuthoringPerspectiveAction_text;

	public static String openBrowsingPerspectiveAction_text;

	public static String editAction_text;

	public static String upgradeLibraryAction_text;

	public static String startup_dir_log_info_text;

	public static String menu_help_software_updates_manage_software_config_text;

	public static String workspaceCannotLockTitle;

	public static String workspaceCannotLockMessage;

	public static String mainActionBarAdvisor_Diagnosis;

	public static String mainActionBarAdvisor_HealthCheck;

	public static String mainActionBarAdvisor_RemoveReference;

	static {
		NLS.initializeMessages(BUNDLE_NAME, RCPUIResources.class);
	}
}