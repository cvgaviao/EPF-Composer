package org.eclipse.epf.authoring.ui.views;

import org.eclipse.epf.authoring.ui.providers.ConfigPackageContentProvider;
import org.eclipse.epf.library.configuration.closure.ConfigurationClosure;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

public class ConfigTreeFilter extends ViewerFilter {
	boolean hideUncheckedNodes = false;

	CheckboxTreeViewer checkTree = null;

	ConfigurationClosure closure = null;

	ConfigPackageContentProvider cp = null;

	public ConfigTreeFilter(ConfigurationClosure closure) {
		this.closure = closure;
	}

	public void setHide() {
		hideUncheckedNodes = hideUncheckedNodes == true ? false : true;
		
		if ( checkTree != null ) {
			checkTree.refresh();
		}
	}

	public boolean select(Viewer viewer, Object parentElement,
			Object element) {
		if (checkTree == null) {
			checkTree = (CheckboxTreeViewer) viewer;
			cp = (ConfigPackageContentProvider) checkTree
					.getContentProvider();
		}

		if (hideUncheckedNodes) {
			Object o = cp.getUITargetElement(element);
			return closure.isSelected(o);
		}
		return true;
	}

	public boolean isHiding() {
		return hideUncheckedNodes;
	}

	public void dispose() {
		checkTree = null;
		cp = null;
		closure = null;
	}

}
