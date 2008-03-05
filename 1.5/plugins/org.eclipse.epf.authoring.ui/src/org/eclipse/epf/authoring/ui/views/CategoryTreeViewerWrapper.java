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
package org.eclipse.epf.authoring.ui.views;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;

/**
 * CheckboxTreeViewer wrapper that does not check parents when all children are checked.
 * 
 * @author Jeff Hardy
 *
 */
public class CategoryTreeViewerWrapper extends
	GrayingCheckboxTreeViewerWrapper2 {
	
	protected Text textWidget;
	
	protected MethodConfiguration config;

	/**
	 * Creates a new instance.
	 */
	public CategoryTreeViewerWrapper(Composite parent, int height,
			Object rootObject,
			AdapterFactoryContentProvider treeContentProvider,
			AdapterFactoryLabelProvider treeLabelProvider,
			MethodConfiguration config) {
		super (parent, height, rootObject, treeContentProvider, treeLabelProvider);
		this.config = config;
	}
		
	public Set<ContentCategory> getCheckedContentCategories() {
		Set<ContentCategory> result = new HashSet<ContentCategory>();
		Set<Object> checkedItems = getWhiteCheckedTreeItems();
		for (Iterator<Object> iter = checkedItems.iterator();iter.hasNext();) {
			Object item = TngUtil.unwrap(iter.next());
			if (item instanceof ContentCategory) {
				if (config.getMethodPluginSelection().contains(LibraryUtil.getMethodPlugin((ContentCategory)item))) {
					result.add((ContentCategory)item);
				}
			}
		}
		return result;
	}
	
	/**
	 *	Handles the selection of an item in the tree viewer
	 *
	 *	@param event ISelection
	 */
	public void selectionChanged(final SelectionChangedEvent event) {
		BusyIndicator.showWhile(getTree().getShell().getDisplay(), new Runnable() {
			public void run() {
				IStructuredSelection selection= (IStructuredSelection) event.getSelection();
				Object selectedElement= selection.getFirstElement();
				selectedElement = TngUtil.unwrap(selectedElement);
				if (selectedElement == null) {
					return;
				} else if (textWidget != null && selectedElement instanceof MethodElement) {
					// display selected element's description
					String briefDesc = ((MethodElement) selectedElement).getBriefDescription();
					textWidget.setText(briefDesc != null ? briefDesc : ""); //$NON-NLS-1$
				}
			}
		});
	}

	public void setTextWidget(Text textWidget) {
		this.textWidget = textWidget;
	}

}
