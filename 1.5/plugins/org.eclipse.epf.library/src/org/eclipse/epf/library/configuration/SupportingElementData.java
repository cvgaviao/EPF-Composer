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

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.closure.ElementReference;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 *  Class managing supporint elements
 * 
 * @author Weiping Lu - Mar 22, 2008
 * @since 1.5
 */
class SupportingElementData {

	private MethodConfiguration config;
	
	private Set<MethodElement> supportingElements;
	private boolean duringUpdateSupporitngElements = false;
	private Set<MethodPlugin> supportingPlugins;
	
	public SupportingElementData(MethodConfiguration config) {		
	}
	
	public void beginUpdateSupportingElements() {
		setDuringUpdateSupporitngElements(true);
		supportingElements = new HashSet<MethodElement>();
		supportingPlugins = new HashSet<MethodPlugin>();
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		for (MethodPlugin plugin: plugins) {
			if (plugin.isSupporting()) {
				supportingPlugins.add(plugin);
			}
		}
	}
	
	//Return list of referred references outside the config 
	public Collection<ElementReference> endUpdateSupportingElements() {
		Collection<ElementReference> refs = new HashSet<ElementReference>();
		Set<MethodElement> newAddedSupportingElements = supportingElements;
		for (MethodElement element: newAddedSupportingElements) {
			
		}		
		setDuringUpdateSupporitngElements(false);
		return refs;
	}
	
	private void collectReferencesOutsideConfig() {
		
	}
		
	public boolean isSupportingElement(MethodElement element) {
		return supportingElements.contains(element);
	}
		
	public boolean pluginIsSupporting(MethodElement element) {
		return pluginIsSupporting(element, null);
	}
	
	private boolean pluginIsSupporting(MethodElement element, Set newAddedSet) {
		if (! isDuringUpdateSupporitngElements()) {
			throw new UnsupportedOperationException();
		}
		if (supportingElements.contains(element)) {
			return true;
		}
		MethodPlugin plugin = UmaUtil.getMethodPlugin(element);
		if (plugin == null) {
			return false;
		}
		if (supportingPlugins.contains(plugin)) {
			supportingElements.add(element);
			if (newAddedSet != null) {
				newAddedSet.add(element);
			}
			return true;
		}
		
		return false;
	}

	private boolean isDuringUpdateSupporitngElements() {
		return duringUpdateSupporitngElements;
	}

	private void setDuringUpdateSupporitngElements(
			boolean duringUpdateSupporitngElements) {
		this.duringUpdateSupporitngElements = duringUpdateSupporitngElements;
	}


	
}
