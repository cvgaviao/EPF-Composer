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
package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.command.INestedCommandProvider;
import org.eclipse.epf.library.edit.command.IUserInteractionHandler;
import org.osgi.framework.Bundle;

/**
 * Helper class with methods to retrieve extensions
 * 
 * @author Phong Nguyen Le - Jun 27, 2006
 * @since  1.0
 */
public final class ExtensionManager {
	private static Map IDToExtensionMap = new HashMap();
	private static List<INestedCommandProvider> nestedCommandProviders;
	private static List oppositeFeatureLoaders;
	
	public static <T>List<T> getExtensions(String namespace, String extensionPointName, Class<T> type) {
		List<T> list = new ArrayList<T>();
		try {
			IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
			IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(namespace, extensionPointName);
			if (extensionPoint != null) {
				IExtension[] extensions = extensionPoint.getExtensions();
				for (int i = 0; i < extensions.length; i++) {
					IExtension extension = extensions[i];
					String pluginId = extension.getNamespaceIdentifier();
					Bundle bundle = Platform.getBundle(pluginId);
					IConfigurationElement[] configElements = extension
					.getConfigurationElements();
					for (int j = 0; j < configElements.length; j++) {
						IConfigurationElement configElement = configElements[j];
						try {
							String className = configElement.getAttribute("class"); //$NON-NLS-1$
							if(className != null) {
								Object ext = bundle.loadClass(className).newInstance();
								if(type.isInstance(ext)) {
									list.add((T)ext);
								}
							}
						} catch (Exception e) {
							LibraryEditPlugin.INSTANCE.log(e);
						}
					}
				}
			}
		}
		catch(Exception e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
		if(list.isEmpty()) {
			return Collections.EMPTY_LIST;
		}
		return list;
	}
	
	public static Object createExtension(String namespace, String extensionPointName) {
		// Process the contributors.
		//
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(namespace, extensionPointName);
		if (extensionPoint != null) {
			IExtension[] extensions = extensionPoint.getExtensions();
			Object ext = null;
			ext_walk:
			for (int i = 0; i < extensions.length; i++) {
				IExtension extension = extensions[i];
				String pluginId = extension.getNamespaceIdentifier();
				Bundle bundle = Platform.getBundle(pluginId);
				IConfigurationElement[] configElements = extension
						.getConfigurationElements();
				for (int j = 0; j < configElements.length; j++) {
					IConfigurationElement configElement = configElements[j];
					try {
						String className = configElement.getAttribute("class"); //$NON-NLS-1$
						if(className != null) {
							ext = bundle.loadClass(className).newInstance();
							break ext_walk;
						}
					} catch (Exception e) {
						LibraryEditPlugin.INSTANCE.log(e);
					}
				}
			}
			return ext;
		}
		return null;
	}
	
	public static Object getExtension(String namespace, String extensionPointName) {
		String ID = namespace + '.' + extensionPointName;
		Object ext = IDToExtensionMap.get(ID);
		if(ext == null) {
			synchronized (IDToExtensionMap) {
				ext = IDToExtensionMap.get(ID);
				if(ext == null) {
					ext = createExtension(namespace, extensionPointName);
					if(ext != null) {
						IDToExtensionMap.put(ID, ext);
					}
				} 
			}

		}
		return ext;
	}

	public static ITextReferenceReplacer getTextReferenceReplacer() {
		return (ITextReferenceReplacer) getExtension(LibraryEditPlugin.getDefault().getId(), "textReferenceReplacer"); //$NON-NLS-1$ 
	}

	public static List<INestedCommandProvider> getNestedCommandProviders() {
		if(nestedCommandProviders == null) {
			nestedCommandProviders = getExtensions(LibraryEditPlugin.getDefault().getId(), 
					"nestedCommandProviders", INestedCommandProvider.class); //$NON-NLS-1$
		}
		return nestedCommandProviders;
	}

	public static IDiagramManager getDiagramManager() {
		return (IDiagramManager) getExtension(LibraryEditPlugin.getDefault().getId(), "diagramManager"); //$NON-NLS-1$ 
	}
	
	public static List getOppositeFeatureLoaders() {
		if(oppositeFeatureLoaders == null) {
			oppositeFeatureLoaders = getExtensions(LibraryEditPlugin.getDefault().getId(), 
					"oppositeFeatureLoaders", INestedCommandProvider.class); //$NON-NLS-1$
		}
		return oppositeFeatureLoaders;
	}
	
	public static IUserInteractionHandler getDefaultUserInteractionHandler() {
		return (IUserInteractionHandler) getExtension(LibraryEditPlugin.getDefault().getId(), "userInteractionHandler"); //$NON-NLS-1$
	}
}