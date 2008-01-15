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
package org.eclipse.epf.authoring.ui.providers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.osgi.framework.Bundle;

/**
 * Manages the <editorPage> elements defined via the
 * "org.eclipse.epf.authoring.ui.EditorPageProvider" extension point.
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public class ProcessEditorPageProvider {

	/**
	 * The extension namespace.
	 */
	public static final String PAGE_PROVIDERS_EXTENSION_NAMESPACE = "org.eclipse.epf.authoring.ui"; //$NON-NLS-1$

	/**
	 * The extension name.
	 */
	public static final String PAGE_PROVIDERS_EXTENSION_NAME = "ProcessEditorPageProvider"; //$NON-NLS-1$

	/**
	 * The extension Attributes
	 */
	public static final String PAGE_PROVIDER_EXTENSION_ATTR_ID = "id"; //$NON-NLS-1$

	public static final String PAGE_PROVIDER_EXTENSION_ATTR_NAME = "name"; //$NON-NLS-1$

	public static final String PAGE_PROVIDER_EXTENSION_ATTR_CLASS = "class"; //$NON-NLS-1$

	// Contains the editor page providers loaded via the
	// "org.eclipse.epf.authoring.ui.ProcessEditorPageProvider" extension point.
	private ArrayList pageProviders = new ArrayList();

	//	 The shared instance.
	private static ProcessEditorPageProvider instance = null;
	
	public static ProcessEditorPageProvider getInstance() {
		if (instance == null) {
			synchronized (ProcessEditorPageProvider.class) {
				if (instance == null) {
					instance = new ProcessEditorPageProvider();
				}
			}
		}
		return instance;
	}

	
	/**
	 * Creates a new instance.
	 */
	private ProcessEditorPageProvider() {

	}

	/**
	 * Returns all the page providers
	 * 
	 * @return all the page providers.
	 */
	public List getPageProviders() {
		List pages = new ArrayList();
		for (int i = 0; i < pageProviders.size(); i++) {
			EditorPageElement pageElement = (EditorPageElement) pageProviders
					.get(i);
			Object contributorClass = null;
			try {
				contributorClass = pageElement.getContributorClass();
			} catch (Exception e) {
			}
			pages.add(contributorClass);
		}
		return pages;
	}
	
	/**
	 * Loads the configuration providers specified via the
	 * "com.ibm.process.pageProviders" extension point.
	 */
	public void loadProviders() {
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(
				PAGE_PROVIDERS_EXTENSION_NAMESPACE,
				PAGE_PROVIDERS_EXTENSION_NAME);
		if (extensionPoint != null) {
			IExtension[] extensions = extensionPoint.getExtensions();
			for (int i = 0; i < extensions.length; i++) {
				IExtension extension = extensions[i];
				String pluginId = extension.getNamespaceIdentifier();
				Bundle bundle = Platform.getBundle(pluginId);
				
				IConfigurationElement[] configElements = extension.getConfigurationElements();
				for (int j = 0; j < configElements.length; j++) {
					IConfigurationElement configElement = configElements[j];
					try {
						String id = configElement
								.getAttribute(PAGE_PROVIDER_EXTENSION_ATTR_ID);
						String name = configElement
								.getAttribute(PAGE_PROVIDER_EXTENSION_ATTR_NAME);
						String contributorClass = configElement
								.getAttribute(PAGE_PROVIDER_EXTENSION_ATTR_CLASS); 
						EditorPageElement pageElement = new EditorPageElement(bundle,
								id, name, contributorClass);
						pageProviders.add(pageElement);
					} catch (Exception e) {
						AuthoringUIPlugin.getDefault().getLogger().logError(
								"Failed to load Editor Page Provider", e); //$NON-NLS-1$
					}
				}
			}
		}
	}
}
