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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.epf.library.edit.validation.DependencyChecker;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.AssociationHelper;

/**
 * utility class to check plugin references
 * @author ???
 * @since 1.0
 */
public class PluginReferenceChecker {

	/**
	 * 
	 * @return List
	 */
//	public static List checkCircularDependentPluginsInLibrary() {
//		List cdList = new ArrayList();
//
//		List allPluginList = ModelStorage.getBaseModels();
//		for (Iterator iter = allPluginList.iterator(); iter.hasNext();) {
//			MethodPlugin element = (MethodPlugin) iter.next();
//			if (hasCircularConflictWithPlugin(element))
//				cdList.add(element);
//		}
//		return cdList;
//	}

	/**
	 * 
	 * @param aPlugin
	 * @return boolean
	 */
	public static boolean hasCircularConflictWithPlugin(MethodPlugin aPlugin) {		
//		boolean answer = false;
//
//		List allowableBaseList = getApplicableBasePlugins(aPlugin);
//		List extendedBaseList = aPlugin.getBases();
//
//		if (!allowableBaseList.containsAll(extendedBaseList)) {
//			System.out
//					.println("$$$: circular dependency detected for " + aPlugin.getName()); //$NON-NLS-1$
//			printPluginList("allowable plugin list", allowableBaseList); //$NON-NLS-1$
//			printPluginList("current base list", extendedBaseList); //$NON-NLS-1$
//			answer = true;
//		}
//
//		return answer;
		
		for (Iterator iter = aPlugin.getBases().iterator(); iter.hasNext();) {
			Object base = (Object) iter.next();
			if(!DependencyChecker.checkCircularDependency(aPlugin, UmaPackage.Literals.METHOD_PLUGIN__BASES, base).isOK()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 * @param aPlugin
	 * @return List
	 */
	public static List getApplicableBasePlugins(MethodPlugin aPlugin) {
		List models = ModelStorage.getBaseModels();

		List descendantList = getDescendants(aPlugin);
		for (Iterator iter = descendantList.iterator(); iter.hasNext();) {
			MethodPlugin element = (MethodPlugin) iter.next();
			if (aPlugin.getBases().contains(element)) {		//part of 170367: allow it to show for unselect
				continue;
			}
			models.remove(element);
		}

		return models;
	}

	// those two getDescendants() methods should be moved into lower layer
	private static List getDescendants(MethodPlugin methodObject) {
		List descendantList = new ArrayList();

		List objList = new ArrayList();
		objList.add(methodObject);

		getDescendants(descendantList, objList);

		return descendantList;
	}

	private static List getDescendants(List allDescendantList,
			List methodObjectList) {
		if (methodObjectList.isEmpty())
			return allDescendantList;

		List combDescendantList = new ArrayList();

		for (Iterator iter = methodObjectList.iterator(); iter.hasNext();) {
			MethodPlugin element = (MethodPlugin) iter.next();
			List descendantList = AssociationHelper
					.getPluginDirectExtensions(element);
			combDescendantList.addAll(descendantList);

		}

		allDescendantList.addAll(methodObjectList);
		List nextCheckList = new ArrayList();
		for (Iterator iter = combDescendantList.iterator(); iter.hasNext();) {
			Object element = iter.next();
			if (!allDescendantList.contains(element))
				nextCheckList.add(element);
		}

		return getDescendants(allDescendantList, nextCheckList);
	}

	/**
	 * 
	 * @param desc
	 * @param pluginList
	 */
	public static void printPluginList(String desc, List pluginList) {
		System.out.print("$$$ " + desc + ": ["); //$NON-NLS-1$ //$NON-NLS-2$
		for (Iterator iterator = pluginList.iterator(); iterator.hasNext();) {
			MethodPlugin plugin = (MethodPlugin) iterator.next();
			System.out.print(plugin.getName() + ", "); //$NON-NLS-1$
		}
		System.out.println("]"); //$NON-NLS-1$
	}

}
