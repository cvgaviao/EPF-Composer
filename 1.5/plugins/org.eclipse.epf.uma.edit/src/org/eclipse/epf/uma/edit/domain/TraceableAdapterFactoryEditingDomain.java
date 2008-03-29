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
package org.eclipse.epf.uma.edit.domain;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.command.CommandStack;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.edit.command.CopyCommand.Helper;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.epf.uma.MethodElement;

/**
 * A traceable adapter factory editing domain used for copying method elements.
 * 
 * @author Phong Nguyen Le
 * @since 1.0
 */
public class TraceableAdapterFactoryEditingDomain extends
		AdapterFactoryEditingDomain {

	// Maps copied method elements to their original counterparts.
	// Note: Copied elements are copies of the clipboard copies.
	private HashMap copyToOriginalMap = null;

	// Maps original elements to their clipboard copies.
	private Map originalToClipboardMap = null;
	
	private Map<EObject, EObject> clipboardToOriginalMap;
	
	// when copy/pasting a plugin, these fields are used by the various copy/add commands for renaming the plugins
	private List<? extends Object> selectedObjectsToCopy = null;
	private boolean createContibuter = false;
	private String newPackageName = null;
	private String newPluginName = null;

	/**
	 * Creates a new instance.
	 * 
	 * @param adapterFactory
	 *            an adapter factory used to create the adapter to which calls
	 *            are delegated
	 * @param commandStack
	 *            a command stack
	 */
	public TraceableAdapterFactoryEditingDomain(AdapterFactory adapterFactory,
			CommandStack commandStack) {
		super(adapterFactory, commandStack);
	}

	/**
	 * Creates a new instance.
	 * 
	 * @param adapterFactory
	 *            an adapter factory used to create the adapter to which calls
	 *            are delegated
	 * @param commandStack
	 *            a command stack
	 * @param resourceToReadOnlyMap
	 *            controls whether the domain is read only
	 */
	public TraceableAdapterFactoryEditingDomain(AdapterFactory adapterFactory,
			CommandStack commandStack, Map resourceToReadOnlyMap) {
		super(adapterFactory, commandStack, resourceToReadOnlyMap);
	}

	/**
	 * Creates a new instance.
	 * 
	 * @param adapterFactory
	 *            an adapter factory used to create the adapter to which calls
	 *            are delegated
	 * @param commandStack
	 *            a command stack
	 * @param resourceSet
	 *            a resource set
	 */
	public TraceableAdapterFactoryEditingDomain(AdapterFactory adapterFactory,
			CommandStack commandStack, ResourceSet resourceSet) {
		super(adapterFactory, commandStack, resourceSet);
	}

	/**
	 * Returns a map containing the copied elements mapped to their original
	 * counterparts.
	 * 
	 * @return a map containing the copied elements mapped to their original
	 *         counterparts
	 */
	public Map getCopyToOriginalMap() {
		if (copyToOriginalMap == null) {
			copyToOriginalMap = new HashMap();
		}
		return copyToOriginalMap;
	}
	
	
	/**
	 * Returns a map containing the original elements mapped to their copied 
	 * counterparts on clipboard
	 * 
	 * @return a map containing the original elements mapped to their copied 
	 *         counterparts
	 */
	public Map getOriginalToClipboardMap() {
		if (originalToClipboardMap == null) {
			originalToClipboardMap = new HashMap();
		}
		return originalToClipboardMap;
	}

	/**
	 * Used by the CreateCopyCommand. Adds infomation to construct maps that
	 * keep track of the original method elements and their copies.
	 * 
	 * @param c
	 *            a collection to store the mapping info
	 * @param copyHelper
	 *            a helper class that is used to keep track of copied objects
	 *            and their associated copies
	 */
	public void addCopyInfo(Collection c, Helper copyHelper) {
		if (isNewCopy(c, copyHelper)) {
			addNewCopy(c, copyHelper);
			return;
		} else {
			chainCopy(c, copyHelper);
			return;
		}
	}

	/**
	 * Adds a new element that is being copied to the clipboard.
	 */
	private void addNewCopy(Collection c, Helper copyHelper) {
		// this means we are copying the Collection c to the clipboard
		if (originalToClipboardMap == null)
			originalToClipboardMap = new HashMap();
		if(clipboardToOriginalMap == null) {
			clipboardToOriginalMap = new HashMap<EObject, EObject>();
		}
		Iterator iter = copyHelper.keySet().iterator();
		while (iter.hasNext()) {
			EObject key = (EObject) iter.next();
			EObject value = (EObject) copyHelper.get(key);
			originalToClipboardMap.put(key, value);
			clipboardToOriginalMap.put(value, key);
		}
	}

	/**
	 * Creates a map from the copied elements to their original counterparts.
	 */
	private void chainCopy(Collection c, Helper copyHelper) {
		// chain the maps - change the copyToOriginalMap
		if (originalToClipboardMap != null) {
			if (copyToOriginalMap == null)
				copyToOriginalMap = new HashMap();
			Iterator iter = originalToClipboardMap.keySet().iterator();
			while (iter.hasNext()) {
				Object o2CKey = iter.next();
				Object o2CValue = originalToClipboardMap.get(o2CKey);
				Object copyValue = copyHelper.get(o2CValue);
				if (copyValue == null) {
					// error
					continue;
				}
				if (o2CKey instanceof MethodElement)
					copyToOriginalMap.put(copyValue, o2CKey);
			}
		}
	}

	private boolean isNewCopy(Collection c, Helper copyHelper) {
		// originalToClipboardMap was set to null at start of copy command
		if (originalToClipboardMap == null)
			return true;

		// iterate through copyHelper map, see if any copyHelper keys are o2C
		// values, which means we are copying clipboard
		Iterator iter = copyHelper.keySet().iterator();
		while (iter.hasNext()) {
			Object clipKey = iter.next();
			if (!originalToClipboardMap.containsValue(clipKey))
				return true;
		}

		return false;
	}

	/**
	 * Initializes the copy maps. It should be called within the copy action.
	 */
	public void resetCopyMaps() {
		if (originalToClipboardMap != null) {
			originalToClipboardMap.clear();
			originalToClipboardMap = null;
		}
		if(clipboardToOriginalMap != null) {
			clipboardToOriginalMap.clear();
			clipboardToOriginalMap = null;
		}
		if (copyToOriginalMap != null) {
			copyToOriginalMap.clear();
			copyToOriginalMap = null;
		}
	}

	public Map<EObject, EObject> getClipboardToOriginalMap() {
		return clipboardToOriginalMap;
	}

	public List<? extends Object> getSelectedObjectsToCopy() {
		return selectedObjectsToCopy;
	}

	public void setSelectedObjectsToCopy(List<? extends Object> selectedObjectsToCopy) {
		this.selectedObjectsToCopy = selectedObjectsToCopy;
	}

	public String getNewPackageName() {
		return newPackageName;
	}

	public void setNewPackageName(String newPackageName) {
		this.newPackageName = newPackageName;
	}

	public String getNewPluginName() {
		return newPluginName;
	}

	public void setNewPluginName(String newPluginName) {
		this.newPluginName = newPluginName;
	}

	public boolean isCreateContibuter() {
		return createContibuter;
	}

	public void setCreateContibuter(boolean createContibuter) {
		this.createContibuter = createContibuter;
	}
}
