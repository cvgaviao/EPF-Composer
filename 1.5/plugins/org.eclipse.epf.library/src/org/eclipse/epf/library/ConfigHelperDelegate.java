package org.eclipse.epf.library;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.persistence.ILibraryResourceSet;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessPackage;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public class ConfigHelperDelegate {

	
	/**
	 * Test if pkg is a system package of plugin
	 * @param plugin
	 * @param pkg
	 * @return
	 */
	public boolean isSystemPackage(MethodPlugin plugin, MethodPackage pkg) {
		return TngUtil.getAllSystemPackages(plugin).contains(pkg);
	}
	
	public void loadOppositeFeatures(ILibraryResourceSet resouceSet,
			List<OppositeFeature> oppositeFeatures,  MethodElement element) {
		Set<String> GUIDs = new HashSet<String>();
		GUIDs.add(element.getGuid());
		resouceSet.loadOppositeFeatures(oppositeFeatures, GUIDs);
	}
	
	public boolean isOwnerSelected(MethodElement element,
			MethodConfiguration config, boolean checkSubtracted) {
		MethodLibrary library = LibraryServiceUtil.getMethodLibrary(config);
		ILibraryManager libraryManager = library == null? null : LibraryService.getInstance().getLibraryManager(library);
		if (libraryManager != null) {
			return LibraryService.getInstance()
						.getConfigurationManager(config)
							.getConfigurationData()
								.isOwnerSelected(element, checkSubtracted);
		}
				
		if (element == null) {
			return false;
		}

		if (config == null || ConfigurationHelper.isDescriptionElement(element)) {
			return true;
		}

		// since UMA 1.0.4, configuration can have added categories 
		// and subtracted categories. The order of filtering is:
		// 1. any element in the subtracted categories should be excluded
		// 2. any element in the added categories should be included
		// 3. any element not in the selected package or plugin should be excluded.
		
		if ( checkSubtracted ) {
			// first check subtracted elements
			List subtractedCategories = config.getSubtractedCategory();
			if ( subtractedCategories.size() > 0 ) {
				for ( Iterator it = subtractedCategories.iterator(); it.hasNext(); ) {
					ContentCategory cc = (ContentCategory)it.next();
					if ( cc == element ) {
						return false;
					}
					
					// need to check all content category types and sub-categories
					// we need to have an efficient algorithm for this checking.
					// for now, only check the custom category's categorised elements
					// TODO. Jinhua Xi, 11/27/2006
					if ( cc instanceof CustomCategory ) {
						if ( ((CustomCategory)cc).getCategorizedElements().contains(element) ) {
							return false;
						}
					} else {
						// TODO, not implemented yet
						System.out.println("TODO, isOwnerSelected: not implemented yet"); //$NON-NLS-1$
					}
				}
			}
		}
		
		// then check added categories
		// TODO
		
		// elements beyond configuration scope should be always visible
		if ((element instanceof MethodLibrary)
				|| (element instanceof MethodConfiguration)) {
			return true;
		} else if (element instanceof MethodPlugin) {
			List plugins = config.getMethodPluginSelection();
			return (plugins != null) && plugins.contains(element);
		} else {
			// if the ownerprocess can't show, can't accept
			if (element instanceof Activity) {
				Activity base = (Activity) ((Activity) element)
						.getVariabilityBasedOnElement();
				if (base != null && base != element) {
					MethodElement owningProc = TngUtil.getOwningProcess(base);
					if ( owningProc != null && owningProc != element 
							&& !ConfigurationHelper.inConfig(owningProc, config, checkSubtracted)) {
						return false;
					}
				}
			}

			EObject pkg = LibraryUtil.getSelectable(element);

			// accept global package if the plugin is in the configuration
			if (pkg instanceof MethodPackage
					&& ConfigurationHelper.isGlobalPackage((MethodPackage) pkg)) {
				MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
				return ConfigurationHelper.inConfig(plugin, config, checkSubtracted);
			}

			List pkgs = config.getMethodPackageSelection();
			if (pkgs == null) {
				return false;
			}

			// per Phong's request, for ProcessPackage, check the
			// ProcessComponent parent instead
			if (pkg instanceof ProcessPackage) {
				while ((pkg != null) && !(pkg instanceof ProcessComponent)
						&& !pkgs.contains(pkg)) {
					pkg = pkg.eContainer();
				}
			}

			// if package not selected, return false
			if ((pkg == null) || !pkgs.contains(pkg)) {
				return false;
			}

			return true;
		}
	}
	
	public void loadOppositeFeatures(Map map, List oppositeFeatures, Set deletedGUIDs) {
		int max = oppositeFeatures.size() - 1;
		if (max < 0) {
			return;
		}
		Collection elements = new HashSet(map.values());
		HashSet loadedElements = new HashSet();
		while (!elements.isEmpty()) {
			for (Iterator iter = elements.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof MethodElement) {
					MethodElement element = (MethodElement) obj;
					MultiResourceEObject mrEObject = ((MultiResourceEObject) element);
					for (int i = max; i > -1; i--) {
						OppositeFeature oppositeFeature = ((OppositeFeature) oppositeFeatures
								.get(i));
						EStructuralFeature eFeature = oppositeFeature.getTargetFeature();
						if (eFeature.getContainerClass().isInstance(element)) {
							if (eFeature.isMany()) {
								InternalEList list = (InternalEList) element
										.eGet(eFeature);
								if (!list.isEmpty()) {
									boolean resolve = false;
									check_resolve: for (Iterator iterator = list
											.basicIterator(); iterator
											.hasNext();) {
										InternalEObject e = (InternalEObject) iterator
												.next();
										if (e.eIsProxy()) {
											String guid = e.eProxyURI()
													.fragment();
											if (deletedGUIDs.contains(guid)) {
												resolve = true;
												break check_resolve;
											}
										}
									}
									if (resolve) {
										Collection<Object> deletedElements = new HashSet<Object>();
										for (Iterator iterator = list
												.iterator(); iterator.hasNext();) {
											Object o = iterator.next();
											if(o instanceof MethodElement && deletedGUIDs.contains(((MethodElement) o).getGuid())) {
												deletedElements.add(o);
											}
										}
										for (Object e : deletedElements) {
											if(oppositeFeature.isMany()) {
												mrEObject.oppositeAdd(oppositeFeature, e);
											}
											else {
												mrEObject.getOppositeFeatureMap().put(oppositeFeature, e);
											}
										}										
									}
								}
							} else {
								Object value = element.eGet(eFeature, false);
								if (value instanceof InternalEObject) {
									InternalEObject e = (InternalEObject) value;
									if (e.eIsProxy()) {
										String guid = e.eProxyURI().fragment();
										if (deletedGUIDs.contains(guid)) {
											Object o = element.eGet(eFeature);
											if(oppositeFeature.isMany()) {
												mrEObject.oppositeAdd(oppositeFeature, o);
											}
											else {
												mrEObject.getOppositeFeatureMap().put(oppositeFeature, o);
											}
										}
									}
								}
							}
						}
					}
				}
			}
			// gets the newly loaded elements to load their opposite features
			//
			loadedElements.addAll(elements);
			elements = new HashSet(map.values());
			elements.removeAll(loadedElements);
		}
	}
	
	public void debugDump(String msg) {
		System.out.println("LD> " + getClass() + ".debugDump: " + msg);	//$NON-NLS-1$//$NON-NLS-2$
	}
}
