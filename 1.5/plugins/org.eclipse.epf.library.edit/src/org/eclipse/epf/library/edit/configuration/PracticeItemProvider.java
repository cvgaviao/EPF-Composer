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
package org.eclipse.epf.library.edit.configuration;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.epf.library.edit.IConfigurable;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.ILibraryItemProvider;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Concept;
import org.eclipse.epf.uma.DescribableElement;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.Roadmap;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;

/**
 * The item provider adapter for a practice.
 * 
 * @author Weiping Lu
 * @since 1.5
 */
public class PracticeItemProvider extends
		org.eclipse.epf.uma.provider.PracticeItemProvider implements
		IConfigurable, ILibraryItemProvider {

	private IFilter filter;

	private Object parent;

	private String label;
	
	private static String flat = "flat"; //$NON-NLS-1$
	private static String categories = "categories"; //$NON-NLS-1$

	public PracticeItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#getChildren(java.lang.Object)
	 */
	public Collection<?> getChildren(Object object) {
		return Collections.EMPTY_LIST;
	}
	
	/**
	 * @param children
	 * @return
	 */
	public Collection<?> getModifiedChildren(Collection children) {
		List ret = new ArrayList();

		Map<String, List> map = getSubGroupMap(children);

		for (Map.Entry<String, List> entry : map.entrySet()) {
			String key = entry.getKey();
			List subgroupChildren = entry.getValue();
			if (key.equals(flat) || key.equals(categories)
					|| subgroupChildren.size() < 3) {
				ret.addAll(subgroupChildren);
			} else {
				PracticeSubgroupItemProvider sub = new PracticeSubgroupItemProvider(
						getAdapterFactory(), key, getImageObject(key), subgroupChildren);
				ret.add(sub);
			}
		}
		
		return ret;
	}
	
	private Map<String, List> getSubGroupMap(Collection children) {
		 Map<String, List> map = new LinkedHashMap<String, List>(); 
		
		for (Object child: children) {
			String key = getSubGroupName(child);
			add(map, key, child);
		}				
		
		return map;
	}
	
	private Object getImageObject(String subGroupName) {
		
		String imageStr = null;
		if (subGroupName.equals(getUIString("_UI_Guidances_Concepts"))) { //$NON-NLS-1$
			imageStr = "full/obj16/Concepts"; //$NON-NLS-1$
		} else if (subGroupName.equals(getUIString("_UI_WorkProducts_group"))) { //$NON-NLS-1$
			imageStr = "full/obj16/WorkProducts"; //$NON-NLS-1$
		} else if (subGroupName.equals(getUIString("_UI_Tasks_group"))) { //$NON-NLS-1$
			imageStr = "full/obj16/Tasks"; //$NON-NLS-1$
		} else if (subGroupName.equals(getUIString("_UI_Roles_group"))) { //$NON-NLS-1$
			imageStr = "full/obj16/Roles"; //$NON-NLS-1$
		} else if (subGroupName.equals(getUIString("_UI_Activities_group"))) { //$NON-NLS-1$
			imageStr = "full/obj16/Processes"; //$NON-NLS-1$
		} else if (subGroupName.equals(getUIString("_UI_Guidances_group"))) { //$NON-NLS-1$
			imageStr = "full/obj16/GuidanceFolder"; //$NON-NLS-1$
		}
		return imageStr == null ? null : LibraryEditPlugin.INSTANCE.getImage(imageStr);
	}
	
	private String getUIString(String key) {
		return LibraryEditPlugin.INSTANCE.getString(key);
	}
	
	private String getSubGroupName(Object obj) {
		if (obj instanceof Roadmap) {
			return flat;
		}
		if (obj instanceof Concept) {
			return getUIString("_UI_Guidances_Concepts"); //$NON-NLS-1$
		}
		if (obj instanceof WorkProduct) {
			return getUIString("_UI_WorkProducts_group"); //$NON-NLS-1$
		}
		if (obj instanceof Task) {
			return getUIString("_UI_Tasks_group"); //$NON-NLS-1$
		}
		if (obj instanceof Role) {
			return getUIString("_UI_Roles_group"); //$NON-NLS-1$
		}
		if (obj instanceof Activity) {
			return getUIString("_UI_Activities_group"); //$NON-NLS-1$
		}
		if (obj instanceof Guidance) {			
			return getUIString("_UI_Guidances_group"); //$NON-NLS-1$
		}
		
		return categories;
	}
	
	private static void add(Map<String, List> map, String key, Object value) {
		List list = map.get(key);
		if (list == null) {
			list = new ArrayList();
			map.put(key, list);
		}
		list.add(value);
	}
	
	public boolean hasChildren(Object object) {
		return true;
	}

	public Collection getNewChildDescriptors(Object object,
			EditingDomain editingDomain, Object sibling) {
		return Collections.EMPTY_LIST;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.epf.uma.provider.PracticeItemProvider#getChildrenFeatures(java.lang.Object)
	 */
	public Collection getChildrenFeatures(Object object) {
		if (childrenFeatures == null) {
			childrenFeatures = new ArrayList();
			childrenFeatures
					.add(UmaPackage.Literals.PRACTICE__CONTENT_REFERENCES);
			childrenFeatures
					.add(UmaPackage.Literals.PRACTICE__ACTIVITY_REFERENCES);
			childrenFeatures.add(UmaPackage.Literals.PRACTICE__SUB_PRACTICES);
		}
		return childrenFeatures;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#getParent(java.lang.Object)
	 */
	public Object getParent(Object object) {
		Object parent = TngUtil
				.getNavigatorParentItemProvider((Guidance) object);
		if (parent == null) {
			return super.getParent(object);
		}
		return parent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.library.edit.IConfigurable#setFilter(com.ibm.library.edit.IFilter)
	 */
	public void setFilter(IFilter filter) {
		this.filter = filter;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.library.edit.IConfigurable#setParent(java.lang.Object)
	 */
	public void setParent(Object parent) {
		this.parent = parent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.library.edit.IConfigurable#setLabel(java.lang.String)
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#getText(java.lang.Object)
	 */
	public String getText(Object object) {
		return TngUtil.getLabel(object, getString("_UI_Practice_type")); //$NON-NLS-1$
	}

	public Object getImage(Object object) {
		if (object instanceof DescribableElement) {
			if (((DescribableElement) object).getNodeicon() != null) {
				URI imgUri = TngUtil.getFullPathofNodeorShapeIconURI(
						(DescribableElement) object,
						((DescribableElement) object).getNodeicon());
				Object image = LibraryEditPlugin.INSTANCE
						.getSharedImage(imgUri);
				if (image != null)
					return image;
			}
		}
		return super.getImage(object);
	}

}