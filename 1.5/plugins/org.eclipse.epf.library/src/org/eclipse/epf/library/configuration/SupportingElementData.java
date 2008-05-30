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
import org.eclipse.epf.library.configuration.closure.ConfigurationClosure;
import org.eclipse.epf.library.configuration.closure.ElementReference;
import org.eclipse.epf.library.edit.util.DebugUtil;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.VariabilityElement;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 *  Class managing supporting elements
 * 
 * @author Weiping Lu - Mar 22, 2008
 * @since 1.5
 */
public class SupportingElementData extends ConfigDataBase {
	
	private Set<MethodElement> supportingElements;
	private boolean duringUpdateSupporitngElements = false;
	private Set<MethodPlugin> supportingPlugins;
	private Set<MethodPackage> selectedPackages;
	private boolean localDebug = false;
	
	public SupportingElementData(MethodConfiguration config) {
		super(config);
	}
	
	public void beginUpdateSupportingElements() {
		setUpdatingChanges(true);
		supportingElements = new HashSet<MethodElement>();
		
		supportingPlugins = new HashSet<MethodPlugin>();
		List<MethodPlugin> plugins = getConfig().getMethodPluginSelection();
		for (MethodPlugin plugin : plugins) {
			if (plugin.isSupporting()) {
				supportingPlugins.add(plugin);
			}
		}

		selectedPackages = new HashSet<MethodPackage>();
		if (! supportingPlugins.isEmpty()) {
			List<MethodPackage> packages = getConfig().getMethodPackageSelection();
			for (MethodPackage pkg : packages) {
				MethodPlugin plugin = UmaUtil.getMethodPlugin(pkg);
				if (supportingPlugins.contains(plugin)) {
					selectedPackages.add(pkg);
				}
			}
		}

	}
	
	// Collect map of referred references outside the config
	public void endUpdateSupportingElements(
			Map<String, ElementReference> outConfigRefMap) {
		
		Set<MethodElement> supportingElementsToCollect = new HashSet<MethodElement>(supportingElements);
		while (!supportingElementsToCollect.isEmpty()) {
			Set<MethodElement> newSupportingElements = new HashSet<MethodElement>();		
			processReferencesOutsideConfig(supportingElementsToCollect, outConfigRefMap, newSupportingElements);
			supportingElementsToCollect = newSupportingElements;
		}
		
		setUpdatingChanges(false);
		setNeedUpdateChanges(false);
	}
	
	private void processReferencesOutsideConfig(
			Collection<MethodElement> elements,
			Map<String, ElementReference> outConfigRefMap, Set<MethodElement> newSupportingElements) {
		for (MethodElement element : elements) {
			processReferencesOutsideConfig(element, outConfigRefMap, newSupportingElements);
		}
	}
	
	private void processReferencesOutsideConfig(MethodElement element,
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
				boolean isOutConfig = checkOutConfigElement(refElement, newSupportingElements);
				if (isOutConfig && outConfigRefMap != null){				
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

	private boolean checkOutConfigElement(MethodElement refElement, Set<MethodElement> newSupportingElements) {
		if (refElement instanceof MethodPackage
				|| refElement instanceof MethodConfiguration) {
			return false;
		}

		if (refElement instanceof VariabilityElement) {
			VariabilityElement replacer = ConfigurationHelper.getReplacer(
					(VariabilityElement) refElement, getConfig());
			if (replacer != null) {
				return false;
			}
		}

		// the element might be subtracted, so ignore it
		if (!ConfigurationHelper.inConfig(refElement, getConfig(), true, false)) {
			return false;
		}

		if (!ConfigurationHelper.inConfig(refElement, getConfig())
				&& !isSupportingElement(refElement, newSupportingElements, true)) {
			return true;
		}

		return false;
	}

	public boolean isSupportingElement(MethodElement element) {
		boolean ret = isSupportingElement_(element);
		if (localDebug) {
			System.out.println("LD> isSE: " + ret + 
					", element: " + DebugUtil.toString(element, 2));//$NON-NLS-1$ ////$NON-NLS-2$ 
		}
		return ret;
	}
	
	
	private boolean isSupportingElement_(MethodElement element) {
		if (isUpdatingChanges()) {				
			return isSupportingElement(element, null, true);			
		} else if (isNeedUpdateChanges()) {
			updateChanges();
		}
		if (supportingElements.isEmpty()) {
			return false;
		}
		
		boolean ret = supportingElements.contains(element);
		if (! ret) {	
			EObject container = LibraryUtil.getSelectable(element);
			if (container instanceof MethodPackage) {
				return isSupportingElement((MethodPackage) container, null, false);
			}
		}
		return ret;
	}
	
	protected void updateChangeImpl() {
		ConfigurationClosure closure = new ConfigurationClosure(null, getConfig());
	}
		
	private boolean isSupportingElement(MethodElement element, Set<MethodElement> newSupportingElements, boolean checkContainer) {
		if (supportingElements.contains(element)) {
			return true;
		}
		
		if (element.getGuid().equals("_avOA0FkVEdul8L-IGeA7TA")) {
			System.out.println("");
		}
		
		EObject container = LibraryUtil.getSelectable(element);
		if (isUpdatingChanges()) {
			boolean ret = false;
			if (container instanceof MethodPackage) {
				ret = selectedPackages.contains(container);
				if (checkContainer && !ret) {
					ret = isSupportingElement((MethodPackage)container, newSupportingElements, false);
				}
			} else if (container instanceof MethodPlugin) {
				ret = supportingPlugins.contains(container);				
			} else if (container instanceof MethodLibrary) {
				ret = supportingPlugins.contains(element);				
			}
			if (ret) {
				supportingElements.add(element);
				if (newSupportingElements != null) {
					newSupportingElements.add(element);
				}
			}
			return ret;
		}
		
		if (checkContainer && container instanceof MethodPackage) {
			return isSupportingElement((MethodPackage)container, newSupportingElements, false);
		}
		
		return false;
	}
		
	public boolean isSupportingSelectable(MethodElement element) {
		return selectedPackages.contains(element) || supportingPlugins.contains(element);
	}

	
}
