//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.configuration;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.util.UmaUtil;


/**
 *  Class managing implicit method configurations
 * 
 * @author Weiping Lu - Mar 12, 2008
 * @since 1.5
 */
public class ImplicitConfigMgr {
	
	private static ImplicitConfigMgr instance = new ImplicitConfigMgr();
	private List<MethodPlugin> newAddedPlugins;
	private MethodConfiguration tempConfig;
	private boolean namespaceMatch = true;
	
	private ImplicitConfigMgr() {		
	}
	
	public static ImplicitConfigMgr getInstance() {
		return instance;
	}
	
	/**
	 * Update config with the selected plug-ins. 
	 * @param config
	 * @param selectedPlugins
	 */
	public void update(MethodConfiguration config, List<MethodPlugin> selectedPlugins) {
		
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		List<MethodPackage> pkgs = config.getMethodPackageSelection();

		plugins.clear();
		pkgs.clear();

		Set<MethodPlugin> addedPlugins = new HashSet<MethodPlugin>();
		Set<MethodPackage> addedPkgs = new HashSet<MethodPackage>();
		
		List<MethodPlugin> matchedSelected = getNamespaceMatched(selectedPlugins);
		
		addPluginsAndPackages(matchedSelected, plugins, pkgs, addedPlugins, addedPkgs);		
	}
	
	private List<MethodPlugin> getNamespaceMatched(List<MethodPlugin> selectedPlugins) {
		if (!namespaceMatch || selectedPlugins.isEmpty()) {
			return selectedPlugins;
		}
		
		MethodLibrary lib = UmaUtil.getMethodLibrary(selectedPlugins.get(0));
		if (lib == null) {
			return selectedPlugins;
		}
		
		List<MethodPlugin> ret = new ArrayList<MethodPlugin>();
		ret.addAll(selectedPlugins);
		Set selectedSet = new HashSet(selectedPlugins);
		for (MethodPlugin selectedPlugin : selectedPlugins) {
			namespaceMatch(ret, lib.getMethodPlugins(), selectedSet);
		}
		
		return ret;
	}

	private void namespaceMatch(List<MethodPlugin> ret,
			List<MethodPlugin> allPlugins, Set selectedSet) {
		for (MethodPlugin plugin : allPlugins) {
			if (selectedSet.contains(plugin)) {
				continue;
			}
			
			String baseName = plugin.getName();			
			int ix = baseName.lastIndexOf("."); //$NON-NLS-1$
			if (ix <= 0) {
				continue;
			}
			
			String matchString = baseName.substring(0, ix + 1);
			String name = plugin.getName();
			if (name.indexOf(matchString) == 0) {
				ret.add(plugin);
				selectedSet.add(plugin);
			}
		}
	}

	private void addPluginsAndPackages(List<MethodPlugin> selectedPlugins,
			List<MethodPlugin> plugins, List<MethodPackage> pkgs,
			Set<MethodPlugin> addedPlugins, Set<MethodPackage> addedPkgs) {
		
		newAddedPlugins = new ArrayList<MethodPlugin>();
		
		for (MethodPlugin plugin: selectedPlugins) {
			addPlugin(addedPlugins, plugin, plugins);
		}		

		for (MethodPlugin plugin: newAddedPlugins) {
			List<MethodPackage> pkgList = plugin.getMethodPackages();
			for (MethodPackage pkg: pkgList) {
				addPackages(addedPkgs, pkg, pkgs);
			}
		}
		
		newAddedPlugins = null;
	}
	
	/**
	 * Remove selected plug-ins from config 
	 * @param config
	 * @param selectedPlugins
	 */
	public void remove(MethodConfiguration config, List<MethodPlugin> selectedPlugins) {
		if (selectedPlugins == null || selectedPlugins.isEmpty()) {
			return;
		}
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		
		boolean needToUpdate = false;
		List<MethodPlugin> finalSelectedPlugins = new ArrayList<MethodPlugin>();
		for (MethodPlugin plugin: plugins) {
			if (selectedPlugins.contains(plugin)) {
				needToUpdate = true;
			} else {
				finalSelectedPlugins.add(plugin);
			}
		}
		
		if (needToUpdate) {
			update(config, finalSelectedPlugins);
		}
	}
	
	/**
	 * Add to config with the selected plug-ins. 
	 * @param config
	 * @param selectedPlugins
	 */
	public void add(MethodConfiguration config, List<MethodPlugin> selectedPlugins) {		
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		List<MethodPackage> pkgs = config.getMethodPackageSelection();
		
		Set<MethodPlugin> addedPlugins = new HashSet<MethodPlugin>(plugins);
		Set<MethodPackage> addedPkgs = new HashSet<MethodPackage>(pkgs);
		
		List<MethodPlugin> matchedSelected = getNamespaceMatched(selectedPlugins);
		
		addPluginsAndPackages(matchedSelected, plugins, pkgs, addedPlugins, addedPkgs);		
	}
	
	private void addPlugin(Set<MethodPlugin> added, MethodPlugin plugin, List<MethodPlugin> plugins) {
		if (added.contains(plugin)) {
			return;
		}
		added.add(plugin);
		newAddedPlugins.add(plugin);
		
		plugins.add(plugin);
		List<MethodPlugin> basePlugins = plugin.getBases();
		for (MethodPlugin basePlugin: basePlugins) {
			addPlugin(added, basePlugin, plugins);
		}
	}
			
	private void addPackages(Set<MethodPackage> added, MethodPackage pkg, List<MethodPackage> pkgs) {
		if (added.contains(pkg)) {
			return;
		}
		added.add(pkg);
		
		pkgs.add(pkg);		
		if (pkg instanceof ProcessComponent) {
			return;
		}
		
		List<MethodPackage> childPkgs = pkg.getChildPackages();
		for (MethodPackage childPkg: childPkgs) {
			addPackages(added, childPkg, pkgs);
		}
	}
		
	public void remove(MethodConfiguration config) {
		MethodLibrary library = LibraryService.getInstance()
		.getCurrentMethodLibrary();
		if (config != null) {
			library.getPredefinedConfigurations().remove(config);
			LibraryService.getInstance().removeConfigurationManager(config);
		}
	}
	
	public void save(MethodConfiguration config) {
		MethodLibrary library = LibraryService.getInstance()
				.getCurrentMethodLibrary();
		ILibraryPersister.FailSafeMethodLibraryPersister persister = LibraryServiceUtil
				.getCurrentPersister().getFailSafePersister();
		
		try {
			persister.save(library.eResource());
			persister.commit();
		} catch (Exception e) {
			try {
				persister.rollback();
			} catch (Exception ex) {
				LibraryPlugin.getDefault().getLogger().logError(ex);
			}
		}
	}
	
	public MethodConfiguration createTemporaryConfiguration(String name, List<MethodPlugin> selectedPlugins) {
		if (tempConfig == null) {
			tempConfig = UmaFactory.eINSTANCE.createMethodConfiguration();
			tempConfig.setName(name); 
		}
		MethodLibrary library = LibraryService.getInstance()
		.getCurrentMethodLibrary();
		library.getPredefinedConfigurations().add(tempConfig);
		update(tempConfig, selectedPlugins);
		return tempConfig;
	}
	
	public MethodConfiguration getTemporaryConfiguration() {
		if (tempConfig == null) {
			return createTemporaryConfiguration("TEMP_PRACTICE", new ArrayList<MethodPlugin>()); //$NON-NLS-1$
		} 
		return tempConfig;
	}
	
	public void clearTemporaryConfiguration() {
		MethodLibrary library = LibraryService.getInstance()
		.getCurrentMethodLibrary();
		library.getPredefinedConfigurations().remove(tempConfig);
		tempConfig = null;
	}
	
	public void updateTemporaryConfiguration(List<MethodPlugin> selectedPlugins) {
		update(getTemporaryConfiguration(), selectedPlugins);
	}
	
	public void addToTemporaryConfiguration(List<MethodPlugin> selectedPlugins) {		
		add(getTemporaryConfiguration(), selectedPlugins);
	}
	
	public void removeFromTemporaryConfiguration(List<MethodPlugin> selectedPlugins) {		
		remove(getTemporaryConfiguration(), selectedPlugins);
	}
		
}
