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
package org.eclipse.epf.library.edit.command;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.command.AbstractCommand;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.epf.library.edit.util.ContentElementOrderList;
import org.eclipse.epf.library.edit.util.Misc;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 * This command is used to reorder a method element within a category.
 * 
 * @author Shashidhar Kannoori
 * @since 1.0
 */
public class MoveInCategoryCommand extends AbstractCommand implements
		IResourceAwareCommand {

	private ContentCategory category;

	private EStructuralFeature feature;

	private List elementsList;
	
	private ContentElementOrderList elementOrderList;

	private String[] categoryPkgPath;

	private ContentCategory usedCategory = null;

	private Collection<Resource> modifiedResources;

	private static int UP = 1;

	private static int Down = 0;

	private int direction = -1;

	private boolean moved;

	public MoveInCategoryCommand(ContentCategory category, List elementsList,
			ContentElementOrderList orderList,
			EStructuralFeature feature, String[] categoryPkgPath,
			int direction) {
		this.category = category;
		this.feature = feature;
		this.elementsList = elementsList;		
		this.elementOrderList = orderList ;
		this.categoryPkgPath = categoryPkgPath;
		this.direction = direction;
		modifiedResources = new HashSet<Resource>();
	}

	protected boolean prepare() {
		return true;
	}

	/**
	 * @param label
	 */
	public MoveInCategoryCommand(String label) {
		super(label);
	}

	/**
	 * @param label
	 * @param description
	 */
	public MoveInCategoryCommand(String label, String description) {
		super(label, description);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.library.edit.command.IResourceAwareCommand#getModifiedResources()
	 */
	public Collection<Resource> getModifiedResources() {
		modifiedResources.add(category.eResource());
		return modifiedResources;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#execute()
	 */
	public void execute() {
		MethodPlugin categoryPlugin = UmaUtil.getMethodPlugin(category);
		MethodPlugin elementPlugin = UmaUtil
				.getMethodPlugin((MethodElement) elementsList.get(0));

		if (categoryPlugin != elementPlugin
				&& Misc.isBaseOf(categoryPlugin, elementPlugin)) {
			usedCategory = TngUtil.findContributor(UmaUtil.findContentPackage(
					elementPlugin, categoryPkgPath), category);
		} else {
			usedCategory = category;
		}
		redo();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#redo()
	 */
	public void redo() {
		if (usedCategory == null)
			return;
		for (Iterator it = elementsList.iterator(); it.hasNext();) {
			Object object = it.next();
			if (feature.isMany()) {
				int index = elementOrderList.indexOf(object);
				if (direction == UP) {
					if (index > 0)
						elementOrderList.move(index - 1, object);
				} else if (direction == Down) {
					if (index < elementOrderList.size())
						elementOrderList.move(index + elementsList.size(), object);
				}
				moved = true;
			} else {
				usedCategory.eSet(feature, null);
			}
		}
		elementOrderList.apply();
	}

	public void undo() {
		if (moved) {
			for (Iterator it = elementsList.iterator(); it.hasNext();) {
				Object object = it.next();
				if (feature.isMany()) {
					int index = elementOrderList.indexOf(object);
					if (direction == UP) {
					if (index < elementOrderList.size())
						elementOrderList.move(index + elementsList.size(), object);
					} else if (direction == Down) {
						if (index > 0)
							elementOrderList.move(index - 1, object);
					}
					moved = true;
				} else {
					usedCategory.eSet(feature, object);
				}
			}
			elementOrderList.apply();
			moved = false;
		}
		if (TngUtil.isEmpty(usedCategory)) {
			EcoreUtil.remove(usedCategory);
			usedCategory = null;
		}
	}

	public Collection<ContentCategory> getAffectedObjects() {
		return Collections.singletonList(usedCategory);
	}

}
