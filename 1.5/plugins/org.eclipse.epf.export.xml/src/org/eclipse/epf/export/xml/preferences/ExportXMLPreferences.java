//------------------------------------------------------------------------------
// Copyright (c) 2004, 2005 IBM Corporation.  All Rights Reserved.
//------------------------------------------------------------------------------
package org.eclipse.epf.export.xml.preferences;

import org.eclipse.epf.export.xml.ExportXMLPlugin;
import org.eclipse.epf.export.xml.services.ExportXMLData;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * The Export XML preferences.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class ExportXMLPreferences {

	/**
	 * The export type preference key.
	 */
	public static final String EXPORT_TYPE = "exportType"; //$NON-NLS-1$

	/**
	 * The exported XML file preference key.
	 */
	public static final String XML_FILE = "xmlFile"; //$NON-NLS-1$

	static {
		// Initialize the default preference values.
		IPreferenceStore store = ExportXMLPlugin.getDefault()
				.getPreferenceStore();
		store.setDefault(EXPORT_TYPE, ExportXMLData.EXPORT_METHOD_LIBRARY); 
		store.setDefault(XML_FILE, ""); //$NON-NLS-1$
	}

	/**
	 * Returns the export type preference.
	 * 
	 * @return the export type preference
	 */
	public static int getExportType() {
		return ExportXMLPlugin.getDefault().getPreferenceStore().getInt(
				EXPORT_TYPE);
	}

	/**
	 * Sets the exported type preference.
	 * 
	 * @param exportType
	 *            the user specified export type
	 */
	public static void setExportType(int exportType) {
		ExportXMLPlugin.getDefault().getPreferenceStore().setValue(EXPORT_TYPE,
				exportType);
	}

	/**
	 * Returns the exported XML file preference.
	 * 
	 * @return the exported XML file preference
	 */
	public static String getXMLFile() {
		return ExportXMLPlugin.getDefault().getPreferenceStore().getString(
				XML_FILE);
	}

	/**
	 * Sets the exported XML file preference.
	 * 
	 * @param path
	 *            the absolute path to a XML file
	 */
	public static void setXMLFile(String path) {
		ExportXMLPlugin.getDefault().getPreferenceStore().setValue(XML_FILE,
				path);
	}

}
