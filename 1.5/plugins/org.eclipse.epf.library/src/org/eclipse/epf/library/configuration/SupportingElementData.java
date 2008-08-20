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
import org.eclipse.epf.library.IConfigurationManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.closure.ConfigurationClosure;
import org.eclipse.epf.library.configuration.closure.ElementReference;
import org.eclipse.epf.library.edit.util.DebugUtil;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.VariabilityElement;
import org.eclipse.epf.uma.VariabilityType;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.util.AssociationHelper;
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
	private static boolean localDebug = false;
	private static boolean localDebug1 = false;
	private boolean enabled = true;
	private Set<VariabilityElement> vChildrenContentCategorySet;
	
	public static boolean descriptorExclusiveOption = true;	
	
	public SupportingElementData(MethodConfiguration config) {
		super(config);
	}
	
	private boolean determineEnable() {
		supportingPlugins = new HashSet<MethodPlugin>();
		Set<MethodPlugin> plugins = new HashSet<MethodPlugin>(getConfig()
				.getMethodPluginSelection());
		for (MethodPlugin plugin : plugins) {
			if (plugin.isSupporting()) {
				supportingPlugins.add(plugin);
			}
		}
		
		setEnabled(supportingPlugins.size() < plugins.size());
		
		return isEnabled();
	}
	
	public void beginUpdateSupportingElements() {
		setUpdatingChanges(true);
		if (localDebug) {
			System.out.println("LD> beginUpdateSupportingElements -> "); //$NON-NLS-1$ 
		}
		
		supportingElements = new HashSet<MethodElement>();
		
		determineEnable();

		selectedPackages = new HashSet<MethodPackage>();
		
		if (isEnabled() && ! supportingPlugins.isEmpty()) {
			List<MethodPackage> packages = getConfig().getMethodPackageSelection();
			for (MethodPackage pkg : packages) {
				MethodPlugin plugin = UmaUtil.getMethodPlugin(pkg);
				if (supportingPlugins.contains(plugin)) {
					selectedPackages.add(pkg);
				}
			}
		}
		vChildrenContentCategorySet = new HashSet<VariabilityElement>();

		if (localDebug) {
			System.out.println("LD> isEnabled(): " + isEnabled()); //$NON-NLS-1$
			System.out.println("LD> supportingPlugins: " + supportingPlugins.size()); //$NON-NLS-1$
			System.out.println("LD> selectedPackages: " + selectedPackages.size()); //$NON-NLS-1$ 
			System.out.println("LD> beginUpdateSupportingElements <- "); //$NON-NLS-1$ 
			System.out.println(""); //$NON-NLS-1$ 
		}		
	}
	
	// Collect map of referred references outside the config
	public void endUpdateSupportingElements(
			Map<String, ElementReference> outConfigRefMap) {
		if (localDebug) {
			System.out.println("LD> endUpdateSupportingElements -> "); //$NON-NLS-1$ 
		}
		if (isEnabled()) {
			Set<MethodElement> supportingElementsToCollect = new HashSet<MethodElement>(supportingElements);
			while (!supportingElementsToCollect.isEmpty()) {
				Set<MethodElement> newSupportingElements = new HashSet<MethodElement>();		
				processReferencesOutsideConfig(supportingElementsToCollect, outConfigRefMap, newSupportingElements);
				if (localDebug) {
					System.out.println("LD> newSupportingElements: " + newSupportingElements.size()); //$NON-NLS-1$
				}	
				supportingElementsToCollect = newSupportingElements;
			}
		}
		
		setUpdatingChanges(false);
		setNeedUpdateChanges(false);
		vChildrenContentCategorySet = null;
		
		if (localDebug) {
			System.out.println("LD> supportingElements: " + supportingElements.size()); //$NON-NLS-1$
			System.out.println("LD> outConfigRefMap: " + outConfigRefMap.size()); //$NON-NLS-1$ 
			System.out.println("LD> endUpdateSupportingElements <- "); //$NON-NLS-1$ 
			System.out.println(""); //$NON-NLS-1$ 
		}	
	}
	
	private void processReferencesOutsideConfig(
			Collection<MethodElement> elements,
			Map<String, ElementReference> outConfigRefMap, Set<MethodElement> newSupportingElements) {
		for (MethodElement element : elements) {
			processVariabilityChildren(element, newSupportingElements);
			if (element instanceof ContentCategory && ! vChildrenContentCategorySet.contains(element)) {
				continue;
			}
			processReferencesOutsideConfig(element, outConfigRefMap, newSupportingElements);
		}
	}
	
	private void processReferencesOutsideConfig(MethodElement element,
			Map<String, ElementReference> outConfigRefMap, Set<MethodElement> newSupportingElements) {
		IConfigurationManager configManager = LibraryService.getInstance().getConfigurationManager(
				getConfig());
		Set<VariabilityElement> replacerSet = null;
		if (configManager != null) {
			replacerSet = configManager.getDependencyManager().getReplacerSet();
		}		
		
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
				if (descriptorExclusiveCheck((MethodElement) value, element, feature)) {
					continue;
				}				
				values = new ArrayList();
				values.add(value);
				
				if (replacerSet != null) {
					if (feature == UmaPackage.eINSTANCE.getVariabilityElement_VariabilityBasedOnElement()) {
						VariabilityElement ve = element instanceof VariabilityElement ?
								(VariabilityElement) element : null;
						VariabilityType type = ve == null ? null : ve.getVariabilityType();
						if (type == VariabilityType.EXTENDS_REPLACES ||
								type == VariabilityType.REPLACES) {
							replacerSet.add(ve);
						}
					}				
				}
				
			}
			
			String guid = element.getGuid();
			for (Object referredValue: values) {
				if (! (referredValue instanceof MethodElement)) {
					continue;
				}
				MethodElement referredElement = (MethodElement)	referredValue;
				boolean isOutConfig = checkOutConfigElement(referredElement, element, newSupportingElements);
				if (isOutConfig && outConfigRefMap != null){				
					String key = guid + referredElement.getGuid();
					ElementReference elementReference = outConfigRefMap.get(key);
					if (elementReference == null) {
						elementReference = new ElementReference(element, referredElement);
						outConfigRefMap.put(key, elementReference);
					}
					elementReference.addFeature(feature);
				}
			}
		}
		
	}
	
	private boolean descriptorExclusiveCheck(MethodElement referredElement, MethodElement referringElement, EStructuralFeature feature) {
		if (! descriptorExclusiveOption) {
			return false;
		}
		
		if (! (referringElement instanceof Descriptor)) {
			return false;
		}
		
		if (feature == UmaPackage.eINSTANCE.getTaskDescriptor_Task()) {
			return referredElement instanceof Task;
		}
		if (feature == UmaPackage.eINSTANCE.getRoleDescriptor_Role()) {
			return referredElement instanceof Role;		
		}
		if (feature == UmaPackage.eINSTANCE.getWorkProductDescriptor_WorkProduct()) {
			return referredElement instanceof WorkProduct;		
		}
		
		return false;
	}

	private boolean checkOutConfigElement(MethodElement referredElement, MethodElement referingElement, Set<MethodElement> newSupportingElements) {
		if (referredElement instanceof MethodPackage
				|| referredElement instanceof MethodConfiguration) {
			return false;
		}

		if (referredElement instanceof VariabilityElement) {
			VariabilityElement replacer = ConfigurationHelper.getReplacer(
					(VariabilityElement) referredElement, getConfig());
			if (replacer != null) {
				return false;
			}
		}

		// the element might be subtracted, so ignore it
		if (!supportingElements.contains(referingElement) && !ConfigurationHelper.inConfig(referingElement, getConfig(), true, false)) {
			return false;
		}

		if (!ConfigurationHelper.inConfig(referredElement, getConfig())
				&& !isOwnerSelected(referredElement, newSupportingElements)) {
			return true;
		}

		return false;
	}

	//ret: 0 = unknown, 1 = yes, 2 = no
	public int checkInConfigIndex(MethodElement element) {
		int ret = checkInConfigIndex_(element);
		if (localDebug1) {
			System.out.println("LD> isSE: " + ret +  //$NON-NLS-1$
					", element: " + DebugUtil.toString(element, 2));//$NON-NLS-1$ 
		}
		return ret;
	}
	
	//ret: 0 = unknown, 1 = yes, 2 = no
	private int checkInConfigIndex_(MethodElement element) {
		if (isUpdatingChanges()) {
			return 2;
			//throw new UnsupportedOperationException();		
		} else if (isNeedUpdateChanges()) {
			updateChanges();
		}
		if (! isEnabled()) {
			return 0;
		}
		if (element instanceof ContentCategory) {
			return 1;
		}		
		return supportingElements.contains(element) ? 1 : 2;
	}
	
	protected void updateChangeImpl() {
		if (determineEnable()) {
			ConfigurationClosure closure = new ConfigurationClosure(null, getConfig());
		}
	}
	
	//isSupportingElement check during updating mode
	public boolean isSupportingElementCallDuringUpdating(ElementReference ref) {
		if (! isEnabled()) {
			return false;
		}
		MethodElement referringElement = ref.getElement();
		MethodElement referredElement = ref.getRefElement();
		EStructuralFeature feature = ref.getSingleFeature();
		if (descriptorExclusiveCheck(referredElement, referringElement, feature)) {
			return false;
		}
		return isOwnerSelected(referredElement, null);
	}
	
	private boolean isOwnerSelected(MethodElement element,
			Set<MethodElement> newSupportingElements) {
		return isOwnerSelected(element, newSupportingElements, true);
	}
		
	private boolean isOwnerSelected(MethodElement element,
				Set<MethodElement> newSupportingElements, boolean register) {
		if (! isUpdatingChanges()) {
			throw new UnsupportedOperationException();	
		}
		
		if (supportingElements.contains(element)) {
			return true;
		}

		boolean ret = false;
		EObject selectable = LibraryUtil.getSelectable(element);
		if (selectable instanceof MethodPackage) {
			ret = selectedPackages.contains(selectable);
		} else if (selectable instanceof MethodPlugin) {
			ret = supportingPlugins.contains(selectable);
		} else if (selectable instanceof MethodLibrary) {
			ret = true;
		}
		if (! register) {
			return ret;
		}
		if (ret) {
			registerAsSupporting(element, newSupportingElements);
		}
		return ret;
	}

	private void registerAsSupporting(MethodElement element,
			Set<MethodElement> newSupportingElements) {
		supportingElements.add(element);
		EObject pkg = element.eContainer();
		while (pkg != null && pkg instanceof MethodPackage) {
			supportingElements.add((MethodPackage) pkg);
			pkg = pkg.eContainer();
		}
		
		if (localDebug1) {
			System.out
					.println("LD> supportingElements added: " + DebugUtil.toString(element, 2));//$NON-NLS-1$ 
		}
		if (newSupportingElements != null) {
			newSupportingElements.add(element);
		}
	}
		
	public boolean isSupportingSelectable(MethodElement element) {
		if (! isEnabled()) {
			return false;
		}
		return selectedPackages.contains(element) || supportingPlugins.contains(element);
	}

	public boolean isEnabled() {
		return enabled;
	}

	private void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public static synchronized boolean isDescriptorExclusiveOption() {
		return descriptorExclusiveOption;
	}

	public static synchronized void setDescriptorExclusiveOption(
			boolean descriptorExclusiveOption) {
		SupportingElementData.descriptorExclusiveOption = descriptorExclusiveOption;
	}
	
	public void processVariabilityChildren(MethodElement elementInConfig,
			Set<MethodElement> newSupportingElements) {
		if (!(elementInConfig instanceof VariabilityElement)) {
			return;
		}

		VariabilityElement base = (VariabilityElement) elementInConfig;
		List<VariabilityElement> vChildren = AssociationHelper
				.getImmediateVarieties(base);
		if (vChildren == null || vChildren.isEmpty()) {
			return;
		}
		
		boolean isContentCategory = base instanceof ContentCategory;
		
		for (VariabilityElement child : vChildren) {
			if (child.getVariabilityBasedOnElement() == base) { // double check
				if (child.getVariabilityType() == VariabilityType.CONTRIBUTES
						|| child.getVariabilityType() == VariabilityType.REPLACES) {
					// child may not be under an supporting plugin -> ok
					if (isOwnerSelected(child, null, false)) {
						List<VariabilityElement> replacers = getReplacers(child);
						if (replacers != null && !replacers.isEmpty()) {
							for (VariabilityElement replacer : replacers) {
								if (isOwnerSelected(replacer,
										newSupportingElements, true)
										&& isContentCategory) {
									vChildrenContentCategorySet.add(child);
								}
							}
						} else {
							registerAsSupporting(child, newSupportingElements);
							if (isContentCategory) {
								vChildrenContentCategorySet.add(child);
							}
						}
					} 

				}
			}
		}

	}
	
	private  List<VariabilityElement> getReplacers(VariabilityElement base) {
		MethodConfiguration config = getConfig();
		
		List<VariabilityElement> vChildren = AssociationHelper
		.getImmediateVarieties(base);
		
		if (vChildren == null || vChildren.isEmpty()) {
			return null;
		}
		
		List<VariabilityElement> replacers = new ArrayList<VariabilityElement>();
		for (VariabilityElement child : vChildren) {
			if (child.getVariabilityBasedOnElement() == base) { // double check
				if (child.getVariabilityType() == VariabilityType.REPLACES) {					
					if (ConfigurationHelper.inConfig(child, config)
							|| isOwnerSelected(child, null, false)) {
						replacers.add(child);
					}
				}
			}
		}
		return replacers;
	}
	
	
}
