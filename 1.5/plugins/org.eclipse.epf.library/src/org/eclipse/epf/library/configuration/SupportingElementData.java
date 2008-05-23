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

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.library.configuration.closure.ElementReference;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 *  Class managing supporting elements
 * 
 * @author Weiping Lu - Mar 22, 2008
 * @since 1.5
 */
class SupportingElementData {

	private MethodConfiguration config;
	
	private Set<MethodElement> supportingElements;
	private boolean duringUpdateSupporitngElements = false;
	private Set<MethodPlugin> supportingPlugins;
	private Set<MethodPackage> supportingPackages;
	
	public SupportingElementData(MethodConfiguration config) {		
	}
	
	public void beginUpdateSupportingElements() {
		setDuringUpdateSupporitngElements(true);
		supportingElements = new HashSet<MethodElement>();
		
		supportingPlugins = new HashSet<MethodPlugin>();
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		for (MethodPlugin plugin : plugins) {
			if (plugin.isSupporting()) {
				supportingElements.add(plugin);
			}
		}

		supportingPackages = new HashSet<MethodPackage>();
		if (! supportingPlugins.isEmpty()) {
			List<MethodPackage> packages = config.getMethodPackageSelection();
			for (MethodPackage pkg : packages) {
				MethodPlugin plugin = UmaUtil.getMethodPlugin(pkg);
				if (supportingPlugins.contains(plugin)) {
					supportingPackages.add(pkg);
				}
			}
		}

	}
	
	// Collect set of referred references outside the config
	public void endUpdateSupportingElements(Set<ElementReference> outConfigRefs) {
		Collection<ElementReference> refs = new HashSet<ElementReference>();
		collectReferencesOutsideConfig(supportingElements, outConfigRefs);
		supportingPlugins = null;
		supportingPackages = null;
		setDuringUpdateSupporitngElements(false);
	}
	
	private void collectReferencesOutsideConfig(Collection<MethodElement> elements, Set<ElementReference> outConfigRefs) {
		for (MethodElement element: elements) {
			
		}		
	}
	
	public boolean isSupportingElement(MethodElement element) {
		return isSupportingElement(element, null);
	}
		
	private boolean isSupportingElement(MethodElement element, Set<MethodElement> newSupporitnElements) {
		if (supportingElements.contains(element)) {
			return true;
		}
		
		boolean ret = false;
		if (isDuringUpdateSupporitngElements()) {
			EObject container = LibraryUtil.getSelectable(element);
			if (container instanceof MethodPackage) {
				ret = supportingPackages.contains(container);
			} else if (container instanceof MethodPlugin) {
				ret = supportingPlugins.contains(container);				
			} else if (container instanceof MethodLibrary) {
				ret = supportingPlugins.contains(element);				
			}
			if (ret) {
				supportingElements.add(element);
				newSupporitnElements.add(element);
			}
		}
		return ret;
	}
		
	public boolean isSupportingSelectable(MethodElement element) {
		return supportingPackages.contains(element) || supportingPlugins.contains(element);
	}
	
	private boolean isDuringUpdateSupporitngElements() {
		return duringUpdateSupporitngElements;
	}

	private void setDuringUpdateSupporitngElements(
			boolean duringUpdateSupporitngElements) {
		this.duringUpdateSupporitngElements = duringUpdateSupporitngElements;
	}


	
}
