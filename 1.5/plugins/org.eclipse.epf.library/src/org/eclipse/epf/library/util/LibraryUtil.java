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
package org.eclipse.epf.library.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.sdo.EProperty;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.LibraryResources;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.TransientGroupItemProvider;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.util.MethodElementPropertyHelper;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.edit.validation.IValidatorFactory;
import org.eclipse.epf.library.persistence.ILibraryResourceSet;
import org.eclipse.epf.library.persistence.PersistenceService;
import org.eclipse.epf.persistence.MultiFileResourceSetImpl;
import org.eclipse.epf.persistence.MultiFileXMISaveImpl;
import org.eclipse.epf.services.Services;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.DescribableElement;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.MethodUnit;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;
import org.eclipse.epf.uma.util.AssociationHelper;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 * @author Jinhua Xi
 * @author Phong Nguyen Le
 * @since 1.0
 */
public class LibraryUtil {

	public static boolean PUBLISH_MODE = false;
	
	private static Comparator<EClass> typeComparator = new Comparator<EClass>() {

		public int compare(EClass o1, EClass o2) {
			return o1.getName().compareTo(o2.getName());
		}
		
	};

	private static List<EClass> includedElementTypes;
	
	private static final Collection<EClass> excludedTypes = Arrays.asList(new EClass[] {
			UmaPackage.eINSTANCE.getBreakdownElement(),
			UmaPackage.eINSTANCE.getCompositeRole(),
			UmaPackage.eINSTANCE.getDescribableElement(),
			UmaPackage.eINSTANCE.getDescriptor(),
			UmaPackage.eINSTANCE.getFulfillableElement(),
			UmaPackage.eINSTANCE.getKind(),
			UmaPackage.eINSTANCE.getProcessComponentDescriptor(),
			UmaPackage.eINSTANCE.getProcessComponentInterface(),
			UmaPackage.eINSTANCE.getProcessElement(),
			UmaPackage.eINSTANCE.getProcessPlanningTemplate(),
			UmaPackage.eINSTANCE.getRoleDescriptor(),
			UmaPackage.eINSTANCE.getTaskDescriptor(),
			UmaPackage.eINSTANCE.getTeamProfile(),
			UmaPackage.eINSTANCE.getWorkBreakdownElement(),
			UmaPackage.eINSTANCE.getWorkOrder(),
			UmaPackage.eINSTANCE.getWorkProductDescriptor()
	});
	
	/**
	 * Check is given plugin name is valid name in the library
	 * 
	 * @param name
	 * @return String
	 */
	public static String checkPluginName(MethodPlugin plugin, String newName) {
		MethodLibrary lib = LibraryService.getInstance()
				.getCurrentMethodLibrary();
		return IValidatorFactory.INSTANCE.createValidator(lib,
				UmaPackage.Literals.METHOD_LIBRARY__METHOD_PLUGINS,
				(IFilter) null, null, UmaPackage.Literals.NAMED_ELEMENT__NAME)
				.isValid(newName);		
	}

	/**
	 * method to check if the element is a plugin or package so that is can be selected in configuration editor. 
	 * May need a better name for this.
	 * @param element EObject
	 * @return boolean
	 */
	public static boolean selectable(EObject element) {
		return (element instanceof MethodLibrary
				|| element instanceof MethodPlugin || element instanceof MethodPackage);
	}

	/**
	 * get the container of the element that is selectable.
	 * @param element EObject
	 * @return EObject
	 */
	public static EObject getSelectable(EObject element) {
		if (element instanceof BreakdownElement) {
			EObject pkg = element.eContainer();
			while (pkg != null && !(pkg instanceof ProcessComponent) ) {
				pkg = pkg.eContainer();
			}
			return pkg;
			
		} else {
			EObject parent = element;
			while ((parent != null) && !selectable(parent)) {
				parent = parent.eContainer();
			}

			return parent;
		}
	}

	/**
	 * get the method plugin for the element
	 * 
	 * @param element EObject
	 * @return MethodPlugin
	 */
	public static MethodPlugin getMethodPlugin(EObject element) {
		// EObject parent = element;
		// while ((parent != null) && !(parent instanceof MethodPlugin)) {
		// parent = parent.eContainer();
		// }
		//
		// return (MethodPlugin) parent;

		return UmaUtil.getMethodPlugin(element);
	}

	/**
	 * get a printable name string for the element. Note, this is not the element name attribute
	 * @param element Object
	 * @return String
	 */
	public static String getName(Object element) {
		if (element == null)
			return LibraryResources.unknown_text; 

		if (element instanceof MethodElement) {
			return getFullName((MethodElement) element);
		}

		return element.toString();
	}

	/**
	 * get a printable name for the element.
	 * @param element MethodElement
	 * @return String
	 */
	public static String getFullName(MethodElement element) {
		if (selectable(element)) {
			StringBuffer buffer = new StringBuffer();
			buffer.append("[").append(element.getName()).append("]"); //$NON-NLS-1$ //$NON-NLS-2$
			MethodElement parent = element;
			while ((parent = (MethodElement) parent.eContainer()) != null) {
				if (parent instanceof MethodLibrary) {
					break;
				}
				buffer.insert(0, "[" + parent.getName() + "]."); //$NON-NLS-1$ //$NON-NLS-2$
			}
			return buffer.toString();
		} else {
			return element.getName();
		}
	}

	/**
	 * get the element's type:name as a localized string
	 * @param element MethodElement
	 * @return String
	 */
	public static String getLocalizeTypeName(MethodElement element) {
		if (element == null) {
			return ""; //$NON-NLS-1$
		}
		if (element instanceof DescribableElement) {
			String nameStr = TngUtil.getTypeText(element) + LibraryResources.colon_with_space ;
			
			if (((DescribableElement) element).getPresentationName() != null) {
				nameStr += "(" + ((DescribableElement) element).getPresentationName() //$NON-NLS-1$
						+ ") " + ((DescribableElement) element).getName(); //$NON-NLS-1$
			} else {
				nameStr += element.getName();
			}
			return nameStr ;
		} else {
			return getTypeName(element);
		}
	}
	
	/**
	 * get the element's type:name
	 * 
	 * @param element MethodElement
	 * @return String
	 */
	public static String getTypeName(MethodElement element) {
		return element == null ? "" : element.getType().getName() + ":" + element.getName(); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/**
	 * get the element's type:name
	 * 
	 * @param element MethodElement
	 * @return String
	 */
	public static String getTypePath(MethodElement element) {
		return element == null ? "" : element.getType().getName() + ":" + TngUtil.getLabelWithPath(element); //$NON-NLS-1$ //$NON-NLS-2$
	}

	
	/**
	 * get the method plugins in the library
	 * @param library MethodLibrary
	 * @return List a list of MethodPlugin objects
	 */
	public static List<MethodPlugin> getMethodPlugins(MethodLibrary library) {
		return new ArrayList<MethodPlugin>(library.getMethodPlugins());
	}

	/**
	 * get a list of guid strings of the method plugins in the library
	 * @param library MethodLibrary
	 * @return List
	 */
	public static List getMethodPluginGuids(MethodLibrary library) {
		List items = new ArrayList();
		List elements = library.getMethodPlugins();
		if (elements != null) {
			for (Iterator it = elements.iterator(); it.hasNext();) {
				MethodPlugin element = (MethodPlugin) it.next();
				items.add(element.getGuid());
			}
		}

		return items;
	}

	/**
	 * get the MethodPlugin in the library with the given plugin guid.
	 * 
	 * @param library MethodLibrary
	 * @param pluginGuid String
	 * @return MethodPlugin
	 */
	public static MethodPlugin getMethodPlugin(MethodLibrary library,
			String pluginGuid) {
		if (pluginGuid == null) {
			return null;
		}

		List elements = library.getMethodPlugins();
		if (elements != null) {
			for (Iterator it = elements.iterator(); it.hasNext();) {
				MethodPlugin element = (MethodPlugin) it.next();
				if (pluginGuid.equals(element.getGuid())) {
					return element;
				}
			}
		}

		return null;
	}
	
	/**
	 * get the MethodPlugin in the library with the given plugin name.
	 * 
	 * @param library MethodLibrary
	 * @param name String
	 * @return MethodPlugin
	 */
	public static MethodPlugin getMethodPluginByName(MethodLibrary library,
			String name) {
		if (name == null) {
			return null;
		}

		List elements = library.getMethodPlugins();
		if (elements != null) {
			for (Iterator it = elements.iterator(); it.hasNext();) {
				MethodPlugin element = (MethodPlugin) it.next();
				if (name.equals(element.getName())) {
					return element;
				}
			}
		}

		return null;
	}


	/**
	 * get all the contained MethodPackages for the element, return empty list if nothing found.
	 * @param element MethodElement
	 * @return List
	 */
	public static List getMethodPackages(MethodElement element) {
		List items = new ArrayList();
		for (Iterator it = element.eAllContents(); it.hasNext();) {
			EObject e = (EObject) it.next();
			if (e instanceof MethodPackage) {
				items.add(e);
			}
		}

		return items;
	}

	/**
	 * upwrap the object
	 * @param obj Object
	 * @return Object
	 */
	public static Object unwrap(Object obj) {
		return TngUtil.unwrap(obj);
	}

	/**
	 * unwrap the command
	 * @param cmd Command
	 * @return Command
	 */
	public static Command unwrap(Command cmd) {
		return TngUtil.unwrap(cmd);
	}

	/**
	 * clear the resource the elements associated to. So that the element can be
	 * added to another library and got new resource assigned.
	 * 
	 * @param importLibraty
	 *            MethodLibrary
	 */
	public static void detachFromResource(MethodLibrary importLibraty) {
		ResourceSet resSet = null;
		Resource res = importLibraty.eResource();
		if (res != null) {
			resSet = res.getResourceSet();
		}

		if (resSet != null) {
			for (TreeIterator it = resSet.getAllContents(); it.hasNext();) {
				Object obj = it.next();
				if (obj instanceof MultiResourceEObject) {
					((MultiResourceEObject) obj).eSetResource(null);
				}
			}
		}

		// clear all the unresolved proxies
		clearProxies(importLibraty);
	}

	/**
	 * clear proxy for the element. This is used to fix the element when the proxy can't be resolved.
	 * @param element EObject
	 */
	public static void clearProxies(EObject element) {
		if (element.eIsProxy()) {
			// reset the proxy to null
			setProxyURI(element, null);
		} else {
			// iterate the children
			for (TreeIterator it = element.eAllContents(); it.hasNext();) {
				EObject o = (EObject) it.next();
				if (o.eIsProxy()) {
					setProxyURI(o, null);
				}
			}
		}
	}

	/**
	 * set the object's proxy uri
	 * @param obj EObject
	 * @param uri org.eclipse.emf.common.util.URI
	 */
	public static void setProxyURI(EObject obj,
			org.eclipse.emf.common.util.URI uri) {
		((org.eclipse.emf.ecore.InternalEObject) obj).eSetProxyURI(uri);
	}

	public static Collection<Resource> getLoadedResources(MethodLibrary lib, Collection<Resource> excludes) {

		Collection<Resource> loadedres = new HashSet<Resource>();

		for (Iterator<Resource> it = lib.eResource().getResourceSet()
				.getResources().iterator(); it.hasNext(); ) {
			Resource res = (Resource)it.next();
			if ( res.isLoaded() ) {
				if ( (excludes == null) || !excludes.contains(res) ) {
					loadedres.add(res);
				}
			}
		}	
		
		return loadedres;
	}
	
	/**
	 * load all elements in the library
	 * @param lib MethodLibrary
	 */
	public static Collection<Resource> loadAll(MethodLibrary lib) {

		Collection<Resource> loadedres = getLoadedResources(lib, null);
		
		loadAllContained(lib);
		
		return getLoadedResources(lib, loadedres);
	}

	public static void loadAllContained(MethodElement me) {
		for (Iterator<EObject> iter = me.eAllContents(); iter.hasNext();) {
			try {
				EObject element = (EObject) iter.next();
				for (Iterator<EObject> iterator = element.eCrossReferences().iterator(); iterator
						.hasNext();) {
					iterator.next();
				}
			} catch (Exception e) {
				LibraryPlugin.getDefault().getLogger().logError(e);
			}
		}
	}
	
	public static void loadAllPlugins(MethodConfiguration config) {
		List<MethodPlugin> pluigns = config.getMethodPluginSelection();
		for (MethodPlugin plugin: pluigns) {
			loadAllContained(plugin);
		}
	}
	
	/**
	 * load all processes in the library
	 * @param lib MethodLibrary
	 */
	public static void loadAllProcesses(MethodLibrary lib) {

		for (Iterator iter = lib.getMethodPlugins().iterator(); iter.hasNext();) {
			try {
				MethodPlugin plugin = (MethodPlugin) iter.next();
				TngUtil.getAllProcesses(plugin);
			} catch (Exception e) {
				LibraryPlugin.getDefault().log(e);
			}
		}
	}
	
	/**
	 * save all elements in the library not matter the element is changed or not. 
	 * Don't refresh the workspace.
	 * @param lib MethodLibrary
	 * @throws Exception
	 */
	public static void saveAll(MethodLibrary lib) throws Exception {
		saveLibrary(lib, true, false);
	}

	/**
	 * save the specified method library based on the library resourceset. You
	 * need to set the resource set before calling this method to save the
	 * library
	 * 
	 * @param lib
	 *            MethodLibrary
	 * @param saveAll
	 *            boolean if true, force saving all the resources even if they
	 *            are not modified.
	 */
	public static void saveLibrary(MethodLibrary lib, boolean saveAll,
			boolean refresh) throws Exception {
		ILibraryResourceSet libResourceSet = (ILibraryResourceSet) lib.eResource().getResourceSet();
		if(libResourceSet instanceof MultiFileResourceSetImpl) {
			MultiFileResourceSetImpl resourceSet = (MultiFileResourceSetImpl) libResourceSet;

			ILibraryManager manager = LibraryService.getInstance()
			.getCurrentLibraryManager();
			Map saveOptions = manager != null ? manager.getSaveOptions()
					: new HashMap();

			// back up current REFRESH_NEW_RESOURCE option
			Object old = saveOptions.get(MultiFileXMISaveImpl.REFRESH_NEW_RESOURCE);
			Object oldCheckModify = saveOptions
			.get(MultiFileXMISaveImpl.CHECK_MODIFY);
			try {
				// disable workspace refresh when new file is created
				saveOptions.put(MultiFileXMISaveImpl.REFRESH_NEW_RESOURCE,
						refresh ? "true" : "false"); //$NON-NLS-1$ //$NON-NLS-2$
				saveOptions.put(MultiFileXMISaveImpl.CHECK_MODIFY, "false"); //$NON-NLS-1$

				// save resource set here
				resourceSet.save(saveOptions, saveAll);
			} finally {
				// restore REFRESH_NEW_RESOURCE option
				saveOptions.put(MultiFileXMISaveImpl.REFRESH_NEW_RESOURCE, old);
				saveOptions.put(MultiFileXMISaveImpl.CHECK_MODIFY, oldCheckModify);
			}
		}
		else {
			libResourceSet.save(Collections.EMPTY_MAP);
		}
	}

	/**
	 * load the library at the specified path. return the MethodLibrary.
	 * This method is different from the the open library in LibraryService. 
	 * It only load the row library object but will not impact the current EPF environment.
	 * @param libraryPath String
	 * @return MethodLibrary
	 * @throws Exception
	 */
	public static MethodLibrary loadLibrary(String libraryPath)
			throws Exception {
		ILibraryResourceSet resourceSet = PersistenceService.INSTANCE.createResourceSet(Services.XMI_PERSISTENCE_TYPE);
		resourceSet.loadMethodLibraries(URI.createFileURI(libraryPath), Collections.EMPTY_MAP);
		return resourceSet.getFirstMethodLibrary();
	}

	/**
	 * get the root path of the method library 
	 * @param lib MethodLibrary
	 * @return File
	 */
	public static File getLibraryRootPath(MethodLibrary lib) {
		Resource res = lib.eResource();
		if (res == null) {
			return null;
		}

		URI uri = res.getURI();
		String path = uri.toFileString();
		File f = new File(path);
		return f.getParentFile();
	}

	/**
	 * get all packages in the plugin
	 * @param plugin MethodPlugin
	 * @return List
	 */
	public static List getAllPackages(MethodPlugin plugin) {
		List allPkgs = new ArrayList();

		List pkgs = plugin.getMethodPackages();
		allPkgs.addAll(pkgs);

		for (Iterator it = pkgs.iterator(); it.hasNext();) {
			getAllChildPackages((MethodPackage) it.next(), allPkgs);
		}

		return allPkgs;
	}

	/**
	 * get all child packages of the given MethodPackage and add to the list
	 * @param pkg MethodPackage
	 * @param result List the packages found
	 */
	public static void getAllChildPackages(MethodPackage pkg, List result) {
		List pkgs = pkg.getChildPackages();
		result.addAll(pkgs);

		for (Iterator it = pkgs.iterator(); it.hasNext();) {
			getAllChildPackages((MethodPackage) it.next(), result);
		}
	}

	/**
	 * get all configurations referenced by this plugin
	 * 
	 * @param plugin
	 * @return List of MethodConfiguration
	 */
	public static List getAssociatedConfigurations(MethodPlugin plugin) {
		// get the configs that references this method plugin
		List allConfigs = new ArrayList();
		List configs = (List) ((MultiResourceEObject) plugin)
				.getOppositeFeatureValue(AssociationHelper.MethodPlugin_MethodConfigurations);
		addUniqueItems(configs, allConfigs);

		// get the configs that references the packages in this plugin
		List pkgs = getAllPackages(plugin);
		for (Iterator it = pkgs.iterator(); it.hasNext();) {
			MultiResourceEObject o = (MultiResourceEObject) it.next();

			configs = (List) o
					.getOppositeFeatureValue(AssociationHelper.MethodPackage_MethodConfigurations);
			addUniqueItems(configs, allConfigs);
		}

		// get the congigurations that referenced by the processes in this
		// plugin
		List procs = TngUtil.getAllProcesses(plugin);
		for (Iterator it = procs.iterator(); it.hasNext();) {
			org.eclipse.epf.uma.Process p = (org.eclipse.epf.uma.Process) it
					.next();
			MethodConfiguration c = p.getDefaultContext();
			if ((c != null) && !allConfigs.contains(c)) {
				allConfigs.add(c);
			}
			addUniqueItems(p.getValidContext(), allConfigs);
		}

		return allConfigs;
	}

	private static void addUniqueItems(List from, List to) {
		if (from == null || to == null || from.size() == 0) {
			return;
		}

		for (Iterator it = from.iterator(); it.hasNext();) {
			Object o = it.next();
			if (!to.contains(o)) {
				to.add(o);
			}
		}
	}

	/**
	 * validate the configuration by forcing to select the global packages of
	 * the selected method plugins, this is needed for configuration exporting.
	 * If global packages are missing, the exported configuration is not valid
	 * 
	 * @param actionMgr if not null, will use the given IActionManager to change the configuration, otherwise configuration will be changed directly
	 * @param plugin
	 */
	public static void validateMethodConfiguration(IActionManager actionMgr, MethodConfiguration config) {
//		List plugins = config.getMethodPluginSelection();
//		List pkgSels = config.getMethodPackageSelection();
//
//		for (Iterator itp = plugins.iterator(); itp.hasNext();) {
//			MethodPlugin plugin = (MethodPlugin) itp.next();
//			List pkgs = TngUtil.getAllSystemPackages(plugin);
//			for (Iterator it = pkgs.iterator(); it.hasNext();) {
//				Object pkg = it.next();
//				if (!pkgSels.contains(pkg)) {
//					pkgSels.add(pkg);
//				}
//			}
//		}
		
		// moved to TngUtil
		TngUtil.validateMethodConfiguration(actionMgr, config);
	}

	/**
	 * validate the configuration by forcing to select the global packages of
	 * the selected method plugins, this is needed for configuration exporting.
	 * If global packages are missing, the exported configuration is not valid
	 * @param plugin
	 */
	public static void validateMethodConfiguration(MethodConfiguration config) {
		validateMethodConfiguration(null, config);
	}
	
	/**
	 * get the copyright object for an element
	 * @param element MethodElement
	 * @return SupportingMaterial
	 */
	public static SupportingMaterial getCopyright(MethodElement element) {
		SupportingMaterial sm = null;
		if (element instanceof MethodUnit) {
			sm = ((MethodUnit) element).getCopyrightStatement();
		} else if (element instanceof DescribableElement) {
			sm = ((DescribableElement) element).getPresentation()
					.getCopyrightStatement();
		}

		// if no copyright of it's own, get the copyright from the plugin
		if (sm == null) {
			MethodPlugin p = getMethodPlugin(element);
			if (p != null) {
				sm = p.getCopyrightStatement();
			}
		}

		return sm;
	}

	/**
	 * for the given container and elements list, check if the element in the list is contained by the container or not. 
	 * return all the contained elements for a given container
	 * @param container Object
	 * @param elements Collection
	 * @return Collection
	 */
	public static Collection getContainedElements(Object container,
			Collection elements) {
		if (container instanceof TransientGroupItemProvider) {
			container = ((TransientGroupItemProvider) container).getTarget();
		}
		ArrayList contained = new ArrayList();
		for (Iterator iter = elements.iterator(); iter.hasNext();) {
			Object element = iter.next();
			if (element instanceof EObject
					&& UmaUtil.isContainedBy((EObject) element, container)) {
				contained.add(element);
			}
		}
		return contained;
	}

	/**
	 * check if two method element are identical or not. Two elements are
	 * identical if and only if: all the attribute values are equal all the
	 * referenced elements are equal all the contained elements are identical
	 * 
	 * @param oldObj
	 *            MethodElement
	 * @param newObj
	 *            MethodElement
	 * @return boolean
	 */
	public static boolean isIdentical(MethodElement oldObj, MethodElement newObj) {

		if ((oldObj == null) && (newObj == null)) {
			return true;
		}

		if ((oldObj == null) || (newObj == null)) {
			return false;
		}

		// this does not work, the toString contains the object instance info
		// return oldObj.toString().equals(newObj.toString());

		List features = getStructuralFeatures(oldObj);
//		List properties = oldObj.getInstanceProperties();
		if (features != null) {
			for (int i = 0; i < features.size(); i++) {
				EStructuralFeature feature = (EStructuralFeature) features
						.get(i);
				Object oldValue = oldObj.eGet(feature);
				Object newValue = newObj.eGet(feature);
				if (oldValue == null && newValue == null) {
					continue;
				}

				if (oldValue == null || newValue == null) {
					return false;
				}

				if (oldValue instanceof MethodElement) {

					// if it'c containment feature value, iterate it
					MethodElement olde = (MethodElement) oldValue;
					if (olde.eContainer() == oldObj) {
						if (!isIdentical(olde, (MethodElement) newValue)) {
							return false;
						}
					} else if (oldValue != newValue) {
						return false;
					}
				} else if (oldValue instanceof List) {
					List oldl = (List) oldValue;
					List newl = (List) newValue;
					if (oldl.size() != newl.size()) {
						return false;
					}

					for (int x = 0; x < oldl.size(); x++) {
						Object o = oldl.get(x);
						Object n = newl.get(x);
						if (o instanceof MethodElement) {
							// if it'c containment feature value, iterate it
							MethodElement olde = (MethodElement) o;
							if (olde.eContainer() == oldObj) {
								if (!isIdentical(olde, (MethodElement) n)) {
									return false;
								}
							} else if (oldValue != newValue) {
								return false;
							}
						} else {
							if (!o.equals(n)) {
								return false;
							}
						}
					}
				} else {
					if (!oldValue.equals(newValue)) {
						return false;
					}
				}
			}
		}

		return true;
	}
	
	/**
	 * check if the element is a process or not. 
	 * A Process is a CapabilityPattern or DeliveryProcess object 
	 * that is contained by a ProcessComponent.
	 * @param e EObject
	 * @return boolean
	 */
	public static boolean isProcess(EObject e) {
		return (e instanceof org.eclipse.epf.uma.Process) && 
			(e.eContainer() instanceof ProcessComponent);
	}
	
	public static List getValidViews(MethodConfiguration config) {
		
		List views = new ArrayList();
		for ( Iterator it = config.getProcessViews().iterator(); it.hasNext(); ) {
			ContentCategory view = (ContentCategory)it.next();
			if ( !ConfigurationHelper.isContributor(view) ) {
				view = (ContentCategory)ConfigurationHelper.getCalculatedElement(view, config);
				if ( view != null && !views.contains(view) ) {
					views.add(view);
				} 
			}	
		}
				
		return views;
	}
	
//	public static List getSuperActivities(BreakdownElement element) {
//		List items = new ArrayList();
//
//		// build the parent nodes
//		BreakdownElement parent = element.getSuperActivities(); 
//		while ( parent != null ) {
//			items.add(0, parent);
//			if (LibraryUtil.isProcess(parent) ) {
//				break;
//			}
//			parent = parent.getSuperActivities();
//		}
//
//		return items;
//	}
	
	/**
	 * Returns all structural features for the given method element
	 * @param element
	 * @return
	 */
	public static List getStructuralFeatures(MethodElement element) {
		List properties = element.getInstanceProperties();

		if (properties != null) {
			List<EStructuralFeature> features = new ArrayList<EStructuralFeature>();
			// get all features
			for (int i = 0; i < properties.size(); i++) {
				EProperty property = (EProperty) properties.get(i);
				if (property != null) {
					EStructuralFeature feature = property
							.getEStructuralFeature();
					features.add(feature);
				}
			}
			return features;
		} else
			return null;
	}
	
	public static List<EClass> getIncludedElementTypes() {
		if (includedElementTypes == null) {
			synchronized (LibraryUtil.class) {
				if (includedElementTypes == null) {
					includedElementTypes = new ArrayList<EClass>();
					for (EClassifier cls : UmaPackage.eINSTANCE
							.getEClassifiers()) {
						if (cls instanceof EClass
								&& UmaPackage.eINSTANCE.getDescribableElement()
										.isSuperTypeOf((EClass) cls)) {
							includedElementTypes.add((EClass) cls);
						}
					}
					includedElementTypes.removeAll(excludedTypes);
					Collections.sort(includedElementTypes, typeComparator);
					includedElementTypes = Collections.unmodifiableList(includedElementTypes);
				}
			}
		}
		return includedElementTypes;
	}
	
	public static List<DescribableElement> getIncludedElements(CustomCategory category, ElementRealizer realizer) {
		List<DescribableElement> includedElements = new ArrayList<DescribableElement>();
		MethodElementProperty prop = MethodElementPropertyHelper.getProperty(category, MethodElementPropertyHelper.CUSTOM_CATEGORY__INCLUDED_ELEMENTS);
		EClassifier cls = UmaPackage.eINSTANCE.getEClassifier(prop.getValue());
		if(cls instanceof EClass) {
			EClass eClass = (EClass) cls;
			if (UmaPackage.eINSTANCE.getDescribableElement().isSuperTypeOf(eClass)) {
				MethodLibrary lib = (MethodLibrary) realizer.getConfiguration()
						.eContainer();
				Iterator<EObject> iter = lib.eAllContents();
				while (iter.hasNext()) {
					EObject eObject = iter.next();
					if (eClass.isInstance(eObject)) {
						includedElements.add((DescribableElement) eObject);
					}
				}
			}
		}
		return includedElements;
	}
}