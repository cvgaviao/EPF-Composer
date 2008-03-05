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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TreeItem;

/**
 * GrayingCheckboxTreeViewerWrapper that does not check parents 
 * when all children are checked.
 * @author Jeff Hardy
 *
 */
public class GrayingCheckboxTreeViewerWrapper2 extends
		GrayingCheckboxTreeViewerWrapper {
	protected List<Object> fUserCheckedStateStore= new ArrayList<Object>();

	/**
	 * Creates a new instance.
	 */
	public GrayingCheckboxTreeViewerWrapper2(Composite parent, int height,
			Object rootObject,
			AdapterFactoryContentProvider treeContentProvider,
			AdapterFactoryLabelProvider treeLabelProvider) {
		super (parent, height, rootObject, treeContentProvider, treeLabelProvider);
	}
	
	@Override
	protected boolean determineShouldBeWhiteChecked(Object treeElement) {
		return fUserCheckedStateStore.contains(treeElement);
	}
	
	protected boolean determineShouldBeAtLeastGrayChecked(Object treeElement) {
		// if any children of treeElement are still gray-checked then treeElement
		// must remain gray-checked as well
		Object[] children= getTreeChildren(treeElement);
		for (int i= 0; i < children.length; ++i) {
			if (fCheckedStateStore.contains(children[i]) || fUserCheckedStateStore.contains(children[i]))
				return true;
		}

		return false;
	}

		
	@Override
	protected void setTreeChecked(Object treeElement, boolean state) {
		if (state) {
			fUserCheckedStateStore.add(treeElement);
		} else
			fUserCheckedStateStore.remove(treeElement);
		
		super.setTreeChecked(treeElement, state);
	}

	
	private Set<TreeItem> getAllTreeItems(TreeItem treeItem) {
		Set<TreeItem> result = new HashSet<TreeItem>();
		if (treeItem != null) {
			result.add(treeItem);
			TreeItem[] children = treeItem.getItems();
			for (int i = 0; i < children.length; i++) {
				result.addAll(getAllTreeItems(children[i]));
			}
		} else {
			TreeItem[] children = fTreeViewer.getTree().getItems();
			for (int i = 0; i < children.length; i++) {
				result.addAll(getAllTreeItems(children[i]));
			}
		}
		return result;
	}
	
	@Override
	protected void handleUpdateSelection(List items) {

		// iterate thru TreeItems to find selections
		Collection<TreeItem> allTreeItems = getAllTreeItems(null);
		Collection<Object> checkedItems = new ArrayList<Object>();
		
		for (Iterator<TreeItem> iter = allTreeItems.iterator();iter.hasNext();) {
			TreeItem treeItem = iter.next();
			Object item = treeItem.getData();
			if (items.contains(TngUtil.unwrap(item))) {
				checkedItems.add(item);
				//Replace the items in the checked state store with those from the supplied items
				if (!fUserCheckedStateStore.contains(item))
					fUserCheckedStateStore.add(item);
				if (!fCheckedStateStore.contains(item))
					fCheckedStateStore.add(item);
				// proceed up the tree element hierarchy
				Object parent= fTreeContentProvider.getParent(item);
				if (parent != null) {
					addToHierarchyToCheckedStore(parent);
				}
				
			}
		}

		//Now update hierarchies
		Iterator<Object> iter = checkedItems.iterator();

		while (iter.hasNext()) {
			Object item= iter.next();
			updateHierarchy(item);
		}
	}
		
	@Override
	protected void treeItemChecked(Object treeElement, boolean state) {
		// uncheck parent but not siblings
		if (!state) {
			Object parent= fTreeContentProvider.getParent(treeElement);
			if (parent != null) {
				fUserCheckedStateStore.remove(parent);
			}
		}
		
		super.treeItemChecked(treeElement, state);
		
		// now check all other instances of the element
		Collection items = getAllTreeItems(treeElement);
		for (Iterator iter = items.iterator();iter.hasNext();) {
			super.treeItemChecked(iter.next(), state);
		}
		
	}
	
	private Collection<Object> getAllTreeItems(Object treeElement) {
		if (treeElement == null) {
			return Collections.<Object>emptyList();
		}
		// iterate thru TreeItems to find selections
		Collection<TreeItem> allTreeItems = getAllTreeItems(null);
		Collection<Object> result = new ArrayList<Object>();
		
		for (Iterator<TreeItem> iter = allTreeItems.iterator();iter.hasNext();) {
			TreeItem treeItem = iter.next();
			Object item = treeItem.getData();
			if (item != treeElement && 
					TngUtil.unwrap(treeElement).equals(TngUtil.unwrap(item))) {
				result.add(item);
			}
		}
		return result;
	}


}
