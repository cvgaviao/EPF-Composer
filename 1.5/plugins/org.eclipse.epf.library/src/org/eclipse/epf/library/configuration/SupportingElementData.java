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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.closure.ElementReference;
import org.eclipse.epf.library.configuration.closure.PackageReference;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.VariabilityElement;
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
	
	// Collect map of referred references outside the config
	public void endUpdateSupportingElements(
			Map<String, ElementReference> outConfigRefMap) {
		
		Set<MethodElement> supportingElementsToCollect = supportingElements;
		while (!supportingElementsToCollect.isEmpty()) {
			Set<MethodElement> newSupportingElements = new HashSet<MethodElement>();		
			collectReferencesOutsideConfig(supportingElementsToCollect, outConfigRefMap, newSupportingElements);
			supportingElementsToCollect = newSupportingElements;
		}
		
		supportingPlugins = null;
		supportingPackages = null;
		setDuringUpdateSupporitngElements(false);
	}
	
	private void collectReferencesOutsideConfig(
			Collection<MethodElement> elements,
			Map<String, ElementReference> outConfigRefMap, Set<MethodElement> newSupportingElements) {
		for (MethodElement element : elements) {
			collectReferencesOutsideConfig(element, outConfigRefMap, newSupportingElements);
		}
	}
	
	private void collectReferencesOutsideConfig(MethodElement element,
			Map<String, ElementReference> outConfigRefMap, Set<MethodElement> newSupportingElements) {
		
		List properties = LibraryUtil.getStructuralFeatures(element);
		for (EStructuralFeature f: (List<EStructuralFeature>) properties) {			
			if (!(f instanceof EReference)) {
				continue;
			}

			EReference feature = (EReference) f;
			if (feature.isContainer() || feature.isContainment()) {
				continue;
			}

			Object value = element.eGet(feature);
			if (value == null) {
				continue;
			}
			
			List values = null;			
			if ( feature.isMany() ) {
				values = (List) value;
			} else if ( value instanceof MethodElement ) {
				values = new ArrayList();
				values.add(value);
			}
			
			String guid = element.getGuid();
			for (Object referredValue: values) {
				if (! (referredValue instanceof MethodElement)) {
					continue;
				}
				MethodElement refElement = (MethodElement)	referredValue;
				if (toAddToOutConfigRefMap(refElement, newSupportingElements)) {				
					String key = guid + refElement.getGuid();
					ElementReference elementReference = outConfigRefMap.get(key);
					if (elementReference == null) {
						elementReference = new ElementReference(element, refElement);
						outConfigRefMap.put(key, elementReference);
					}
					elementReference.addFeature(feature);
				}
			}
		}
		
	}

	private boolean toAddToOutConfigRefMap(MethodElement refElement, Set<MethodElement> newSupportingElements) {
		if (refElement instanceof MethodPackage
				|| refElement instanceof MethodConfiguration) {
			return false;
		}

		if (refElement instanceof VariabilityElement) {
			VariabilityElement replacer = ConfigurationHelper.getReplacer(
					(VariabilityElement) refElement, config);
			if (replacer != null) {
				return false;
			}
		}

		// the element might be subtracted, so ignore it
		if (!ConfigurationHelper.inConfig(refElement, config, true, false)) {
			return false;
		}

		if (!ConfigurationHelper.inConfig(refElement, config)
				&& !isSupportingElement(refElement, newSupportingElements)) {
			return true;
		}

		return false;
	}

	public boolean isSupportingElement(MethodElement element) {
		return isSupportingElement(element, null);
	}
		
	private boolean isSupportingElement(MethodElement element, Set<MethodElement> newSupportingElements) {
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
				newSupportingElements.add(element);
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
