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
package org.eclipse.epf.authoring.ui.providers;

import java.util.Set;

import org.eclipse.epf.authoring.ui.views.CategoryTreeViewerWrapper;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

/**
 * Filter for Custom Category view of configuration page
 * 
 * @author Jeff Hardy
 */
public class CategoryTreeFilter extends ViewerFilter {
	
	CategoryTreeViewerWrapper CCViewer;
	
	private boolean hideUnchecked = false;
	
	public CategoryTreeFilter(CategoryTreeViewerWrapper CCViewer) {
		this.CCViewer = CCViewer;
	}

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (hideUnchecked && !isChecked(element))
			return false;
		return true;
	}

	public boolean isHideUnchecked() {
		return hideUnchecked;
	}

	public void toggleHideUnchecked() {
		hideUnchecked = !hideUnchecked;
	}
	
	private boolean isChecked(Object element) {
		Set<Object> checkedItems = CCViewer.getAllCheckedTreeItems();
		return checkedItems.contains(element);
	}
}
