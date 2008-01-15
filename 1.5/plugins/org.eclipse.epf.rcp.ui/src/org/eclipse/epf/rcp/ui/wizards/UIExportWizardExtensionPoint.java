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
package org.eclipse.epf.rcp.ui.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.rcp.ui.RCPUIPlugin;

/**
 * Manages the 'org.eclipse.epf.ui.exportWizards" extension point.
 * <p>
 * This extension point allows a contributing plug-in to add its export wizards
 * to the standalone RCP-based EPF Composer Export wizard page.
 * 
 * @author Shashidhar Kannoori
 * @author Kelvin Low
 * @since 1.0
 */
public class UIExportWizardExtensionPoint {

	/**
	 * The extension namespace.
	 */
	public static final String PAGE_PROVIDERS_EXTENSION_NAMESPACE = "org.eclipse.epf.ui"; //$NON-NLS-1$

	/**
	 * The extension name.
	 */
	public static final String PAGE_PROVIDERS_EXTENSION_NAME = "exportWizards"; //$NON-NLS-1$

	/**
	 * The extension Attributes
	 */
	public static final String PAGE_PROVIDER_EXTENSION_ATTR_ID = "id"; //$NON-NLS-1$

	private ArrayList extensionsList = new ArrayList();

	// The shared instance.
	private static UIExportWizardExtensionPoint instance = null;

	public static UIExportWizardExtensionPoint getInstance() {
		if (instance == null) {
			synchronized (UIImportWizardExtensionPoint.class) {
				if (instance == null) {
					instance = new UIExportWizardExtensionPoint();
				}
			}
		}
		return instance;
	}

	/**
	 * Creates a new instance.
	 */
	private UIExportWizardExtensionPoint() {
	}

	/**
	 * Returns all the page providers
	 * 
	 * @return all the page providers.
	 */
	public List getPageProviders() {
		return extensionsList;
	}

	/**
	 * Loads the configuration providers specified via the
	 * "org.eclipse.epf.ui.exportWizards" extension point.
	 */
	public void loadExtension() {
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(
				PAGE_PROVIDERS_EXTENSION_NAMESPACE,
				PAGE_PROVIDERS_EXTENSION_NAME);
		if (extensionPoint != null) {
			IExtension[] extensions = extensionPoint.getExtensions();
			for (int i = 0; i < extensions.length; i++) {
				IExtension extension = extensions[i];
				IConfigurationElement[] configElements = extension
						.getConfigurationElements();
				for (int j = 0; j < configElements.length; j++) {
					IConfigurationElement configElement = configElements[j];
					try {
						String id = configElement
								.getAttribute(PAGE_PROVIDER_EXTENSION_ATTR_ID);
						extensionsList.add(id);

					} catch (Exception e) {
						RCPUIPlugin
								.getDefault()
								.getLogger()
								.logError(
										"Failed to export import contribution extension points", e); //$NON-NLS-1$
					}
				}
			}
		}
	}

}
