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
import org.eclipse.epf.authoring.ui.forms.IExtensionEditorPart;
import org.osgi.framework.Bundle;

/**
 * Manages the <editorPage> elements defined via the
 * "org.eclipse.epf.authoring.ui.EditorPageProvider" extension point.
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public class MethodEditorPageProvider {

	/**
	 * The extension namespace.
	 */
	public static final String PAGE_PROVIDERS_EXTENSION_NAMESPACE = "org.eclipse.epf.authoring.ui"; //$NON-NLS-1$

	/**
	 * The extension name for task pages
	 */
	public static final String TASK_PAGE_PROVIDERS_EXTENSION_NAME = "TaskEditorPageProvider"; //$NON-NLS-1$

	/**
	 * The extension name for custom category pages
	 */
	public static final String CUSTOMCATEGORY_PAGE_PROVIDERS_EXTENSION_NAME = "CustomCategoryEditorPageProvider"; //$NON-NLS-1$
	
	
	/**
	 * The extension Attributes
	 */
	public static final String PAGE_PROVIDER_EXTENSION_ATTR_ID = "id"; //$NON-NLS-1$

	public static final String PAGE_PROVIDER_EXTENSION_ATTR_NAME = "name"; //$NON-NLS-1$

	public static final String PAGE_PROVIDER_EXTENSION_ATTR_CLASS = "class"; //$NON-NLS-1$

	// Contains the editor page providers loaded via the
	// "org.eclipse.epf.authoring.ui.MethodEditorPageProvider" extension point.
	private List<EditorPageElement> taskPageProviders = new ArrayList<EditorPageElement>();
	private List<EditorPageElement> customCategoryPageProviders = new ArrayList<EditorPageElement>();

	//	 The shared instance.
	private static MethodEditorPageProvider instance = null;
	
	public static MethodEditorPageProvider getInstance() {
		if (instance == null) {
			synchronized (MethodEditorPageProvider.class) {
				if (instance == null) {
					instance = new MethodEditorPageProvider();
				}
			}
		}
		return instance;
	}

	/**
	 * Creates a new instance.
	 */
	private MethodEditorPageProvider() {

	}

	/**
	 * Returns all the task page providers
	 * 
	 * @return all the task page providers.
	 */
	public List<IExtensionEditorPart> getTaskPageProviders() {
		List<IExtensionEditorPart> pages = new ArrayList<IExtensionEditorPart>();
		for (int i = 0; i < taskPageProviders.size(); i++) {
			EditorPageElement pageElement = (EditorPageElement) taskPageProviders
					.get(i);
			IExtensionEditorPart contributorClass = null;
			try {
				contributorClass = pageElement.getContributorClass();
			} catch (Exception e) {
			}
			pages.add(contributorClass);
		}
		return pages;
	}

	/**
	 * Returns all the custom category page providers
	 * 
	 * @return all the customcategory page providers.
	 */
	public List<IExtensionEditorPart> getCustomCategoryPageProviders() {
		List<IExtensionEditorPart> pages = new ArrayList<IExtensionEditorPart>();
		for (int i = 0; i < customCategoryPageProviders.size(); i++) {
			EditorPageElement pageElement = (EditorPageElement) customCategoryPageProviders
					.get(i);
			IExtensionEditorPart contributorClass = null;
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
		loadProvider(TASK_PAGE_PROVIDERS_EXTENSION_NAME, taskPageProviders);
		loadProvider(CUSTOMCATEGORY_PAGE_PROVIDERS_EXTENSION_NAME, customCategoryPageProviders);
	}
	
	private void loadProvider(String extName, List<EditorPageElement> list) {
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(
				PAGE_PROVIDERS_EXTENSION_NAMESPACE,
				extName);
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

						EditorPageElement pageElement = new EditorPageElement(
								bundle, id, name, contributorClass);
						list.add(pageElement);
						
					} catch (Exception e) {
						AuthoringUIPlugin.getDefault().getLogger().logError(
								"Failed to load Editor Page Provider", e); //$NON-NLS-1$
					}
				}
			}
		}

	}
}
