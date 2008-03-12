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


/**
 *  Class managing implicit method configurations
 * 
 * @author Weiping Lu - Mar 12, 2008
 * @since 1.5
 */
public class ImplicitConfigMgr {
	
	private static ImplicitConfigMgr instance = new ImplicitConfigMgr();
	
	private ImplicitConfigMgr() {		
	}
	
	public static ImplicitConfigMgr getInstance() {
		return instance;
	}
	
	public void update(MethodConfiguration config, List<MethodPlugin> selectedPlugins) {
		
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		List<MethodPackage> pkgs = config.getMethodPackageSelection();

		plugins.clear();
		pkgs.clear();

		Set<MethodPlugin> addedPlugins = new HashSet<MethodPlugin>();
		for (MethodPlugin plugin: selectedPlugins) {
			addPlugin(addedPlugins, plugin, plugins);
		}
		
		Set<MethodPackage> addedPkgs = new HashSet<MethodPackage>();
		for (MethodPlugin plugin: plugins) {
			List<MethodPackage> pkgList = plugin.getMethodPackages();
			for (MethodPackage pkg: pkgList) {
				addPackages(addedPkgs, pkg, pkgs);
			}
		}		
	}
	
	private void addPlugin(Set<MethodPlugin> added, MethodPlugin plugin, List<MethodPlugin> plugins) {
		if (added.contains(plugin)) {
			return;
		}
		added.add(plugin);
		
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
		
}
