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
package org.eclipse.epf.library.edit.navigator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.command.CopyCommand.Helper;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.epf.library.edit.ILibraryItemProvider;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.PluginUIPackageContext;
import org.eclipse.epf.library.edit.command.MethodElementAddCommand;
import org.eclipse.epf.library.edit.util.Comparators;
import org.eclipse.epf.library.edit.util.PluginUIPackagesMap;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.edit.command.MethodElementCreateCopyCommand;
import org.eclipse.epf.uma.edit.command.MethodElementInitializeCopyCommand;
import org.eclipse.epf.uma.provider.MethodPluginItemProvider;

/**
 * The item provider adapter for a method library.
 * 
 * @author Phong Nguyen Le
 * @author Kelvin Low
 * @since 1.0
 */
public class MethodLibraryItemProvider extends
		org.eclipse.epf.uma.provider.MethodLibraryItemProvider {

	private ConfigurationsItemProvider configurations;
	
	private Map<String, PluginUIPackagesItemProvider> pluginPackagesItemProvidersMap = new HashMap<String, PluginUIPackagesItemProvider>();
	

	//private ProcessFamiliesItemProvider procFamilies;

	/**
	 * Creates a new instance.
	 */
	public MethodLibraryItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#getChildrenFeatures(java.lang.Object)
	 */
	public Collection getChildrenFeatures(Object object) {
		if (childrenFeatures == null) {
			childrenFeatures = new ArrayList();
			childrenFeatures.add(UmaPackage.eINSTANCE
					.getMethodLibrary_MethodPlugins());
		}
		return childrenFeatures;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#collectNewChildDescriptors(java.util.Collection,
	 *      java.lang.Object)
	 */
	protected void collectNewChildDescriptors(Collection newChildDescriptors,
			Object object) {
		newChildDescriptors.add(createChildParameter(UmaPackage.eINSTANCE
				.getMethodLibrary_MethodPlugins(), UmaFactory.eINSTANCE
				.createMethodPlugin()));
	}

	public Collection getChildren(Object object) {
		List children;
		if (object instanceof MethodLibrary && !PluginUIPackageContext.INSTANCE.isFlatLayout()) {
			children = new ArrayList();
			List<MethodPlugin> plugins = getMethodPlugins((MethodLibrary)object);
			addChildren(children, plugins, object);
			Collections.sort(children, Comparators.PLUGINPACKAGE_COMPARATOR);
		} else { 
			Collection collection = super.getChildren(object);
			if (collection instanceof List) {
				children = (List) collection;
			} else {
				children = new ArrayList(collection);
			}
			// set parents
			for (Iterator iter = children.iterator(); iter.hasNext();) {
				Object adapter = adapterFactory.adapt(iter.next(),
						ITreeItemContentProvider.class);
				if (adapter instanceof ILibraryItemProvider) {
					((ILibraryItemProvider) adapter).setParent(this);
				}
			}
			
			Collections.sort(children, Comparators.PLUGINPACKAGE_COMPARATOR);
		}
		if (configurations == null) {
			configurations = new ConfigurationsItemProvider(adapterFactory,
					(MethodLibrary) object, LibraryEditPlugin.INSTANCE
							.getString("_UI_Configurations_text")); //$NON-NLS-1$
		}
		children.add(configurations);

		// Remove 'Process Families' until it is fully implemented
		//
		// if(procFamilies == null) {
		// procFamilies = new ProcessFamiliesItemProvider(adapterFactory,
		// (MethodLibrary) object,
		// LibraryEditPlugin.INSTANCE.getString("_UI_ProcessFamilies_text"));
		// }
		// children.add(procFamilies);
		

		return children;
	}
	
	private void addChildren(Collection children, List<MethodPlugin> plugins, Object parent) {
		PluginUIPackagesMap map = new PluginUIPackagesMap();
		for (Iterator<MethodPlugin> iter = plugins.iterator();iter.hasNext();) {
			MethodPlugin plugin = iter.next();
			String pluginName = plugin.getName();
			int dotIdx = pluginName.indexOf(PluginUIPackagesItemProvider.PLUGIN_PACKAGE_SEPARATOR);
			if (dotIdx != -1) {
				map.add(pluginName.substring(0, dotIdx), plugin);
			} else {
				children.add(plugin);
				Object adapter = adapterFactory.adapt(plugin,
						ITreeItemContentProvider.class);
				if (adapter instanceof ILibraryItemProvider) {
					((ILibraryItemProvider) adapter).setParent(this);
				}
			}
		}
		Collection<String> existingNames = new HashSet<String>();
		for (Iterator<Map.Entry<String, Set<MethodPlugin>>> iter = map.entrySet().iterator();iter.hasNext();) {
			Map.Entry<String, Set<MethodPlugin>> entry = iter.next();
			String name = entry.getKey();
			Set<MethodPlugin> plugSet = entry.getValue();
			PluginUIPackagesItemProvider provider = pluginPackagesItemProvidersMap.get(name);
			if (provider == null) {
				provider = new PluginUIPackagesItemProvider(adapterFactory, name, plugSet);
				pluginPackagesItemProvidersMap.put(name, provider);
			} else {
				provider.setPlugins(plugSet);
			}
			existingNames.add(name);
			provider.setParent(parent);
			children.add(provider);
		}
		retainPluginPackagesItemProviders(existingNames);
	}
	
	private void retainPluginPackagesItemProviders(Collection<String> names) {
		for (Iterator<String> iter = pluginPackagesItemProvidersMap.keySet().iterator();iter.hasNext();) {
			String keyName = iter.next();
			if (!names.contains(keyName)) {
				PluginUIPackagesItemProvider provider = pluginPackagesItemProvidersMap.get(keyName);
				if (provider != null) {
					provider.dispose();
				}
				iter.remove();
			}
		}
	}

	private static List<MethodPlugin> getMethodPlugins(MethodLibrary library) {
		List<MethodPlugin> items = new ArrayList<MethodPlugin>();
		EList elements = library.eContents();
		if (elements != null) {
			for (Iterator it = elements.iterator(); it.hasNext();) {
				EObject element = (EObject) it.next();
				if (element instanceof MethodPlugin) {
					items.add((MethodPlugin)element);
				}
			}
		}
		return items;
	}


	public Object getConfigurations() {
		return configurations;
	}

	public Object getProcessFamilies() {
		//return procFamilies;
		return null;
	}

	protected Command createInitializeCopyCommand(EditingDomain domain,
			EObject owner, Helper helper) {
		return new MethodElementInitializeCopyCommand(domain, owner, helper);
	}

	protected Command createCreateCopyCommand(EditingDomain domain,
			EObject owner, Helper helper) {
		return new MethodElementCreateCopyCommand(domain, owner, helper);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#createAddCommand(org.eclipse.emf.edit.domain.EditingDomain, org.eclipse.emf.ecore.EObject, org.eclipse.emf.ecore.EStructuralFeature, java.util.Collection, int)
	 */
	protected Command createAddCommand(EditingDomain domain, EObject owner, EStructuralFeature feature, Collection collection, int index) {
		return new MethodElementAddCommand(super.createAddCommand(domain, owner, feature, collection, index));
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#dispose()
	 */
	public void dispose() {
		if (configurations != null) {
			configurations.dispose();
			configurations = null;
		}
		
		if (pluginPackagesItemProvidersMap != null) {
			pluginPackagesItemProvidersMap.clear();
			pluginPackagesItemProvidersMap = null;
		}
		
		/*
		if (procFamilies != null) {
			procFamilies.dispose();
			procFamilies = null;
		}
		*/
		super.dispose();
	}
	
	/**
	 * 
	 * @param plugin
	 * @return the ItemProviderAdapter for the plugin - either a MethodPluginItemProvider or a PluginUIPackagesItemProvider. null if none found
	 */
	public ItemProviderAdapter getPluginItemProvider(MethodPlugin plugin) {
		String pluginName = plugin.getName();
		int dotIdx = pluginName.indexOf(PluginUIPackagesItemProvider.PLUGIN_PACKAGE_SEPARATOR);
		if (dotIdx != -1) {
			String packageProviderName = pluginName.substring(0, dotIdx);
			if (pluginPackagesItemProvidersMap != null) {
				PluginUIPackagesItemProvider provider = pluginPackagesItemProvidersMap.get(packageProviderName);
				if (provider != null) {
					return provider;
				}
			}			
		} else {
			return (MethodPluginItemProvider) TngUtil
				.getAdapter(plugin, MethodPluginItemProvider.class);
		}
		return null;
	}
}
