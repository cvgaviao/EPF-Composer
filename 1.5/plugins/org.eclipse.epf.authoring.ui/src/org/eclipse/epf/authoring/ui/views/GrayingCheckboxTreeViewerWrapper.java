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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;

/**
 * Wrapper around a org.eclipse.jface.viewers.CheckboxTreeViewer that supports graying of
 * parent elements with partial child selections
 * 
 * code taken from org.eclipse.jdt.internal.ui.jarpackager.CheckboxTreeAndListGroup
 * @author Jeff Hardy
 *
 */
public class GrayingCheckboxTreeViewerWrapper implements ICheckStateListener, ISelectionChangedListener, ITreeViewerListener {
	
	protected AdapterFactoryContentProvider fTreeContentProvider;
	protected AdapterFactoryLabelProvider fTreeLabelProvider;
	
	protected List<Object> fExpandedTreeNodes= new ArrayList<Object>();
	protected List<Object> fCheckedStateStore= new ArrayList<Object>();
	protected List<Object> fWhiteCheckedTreeItems= new ArrayList<Object>();
	protected List<ICheckStateListener> fListeners= new ArrayList<ICheckStateListener>();
	
	// widget
	protected CheckboxTreeViewer fTreeViewer;
	
	protected Object fRoot;

	/**
	 * Creates a new instance.
	 */
	public GrayingCheckboxTreeViewerWrapper(Composite parent, int height,
			Object rootObject,
			AdapterFactoryContentProvider treeContentProvider,
			AdapterFactoryLabelProvider treeLabelProvider) {
		
		fRoot = rootObject;
		fTreeContentProvider = treeContentProvider;
		fTreeLabelProvider = treeLabelProvider;
		createContents(parent, height);
	}
	
	/**
	 *	Lay out and initialize self's visual components.
	 *
	 *	@param parent org.eclipse.swt.widgets.Composite
	 *	@param width int
	 *	@param height int
	 */
	protected void createContents(Composite parent, int height) {
		createTreeViewer(parent, height);
		initialize();
	}

	protected void createTreeViewer(Composite parent, int height) {
		Tree tree= new Tree(parent, SWT.CHECK | SWT.BORDER);
		GridData data= new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL);
		data.heightHint= height;
		tree.setLayoutData(data);

		fTreeViewer= new CheckboxTreeViewer(tree);
		fTreeViewer.setUseHashlookup(true);
		fTreeViewer.setContentProvider(fTreeContentProvider);
		fTreeViewer.setLabelProvider(fTreeLabelProvider);
		fTreeViewer.addTreeListener(this);
		fTreeViewer.addCheckStateListener(this);
		fTreeViewer.addSelectionChangedListener(this);
	}

	/**
	 * This method must be called just before this window becomes visible.
	 */
	public void aboutToOpen() {
		determineWhiteCheckedDescendents(fRoot);
		checkNewTreeElements(getTreeChildren(fRoot));

		//select the first element in the list
		Object[] elements= getTreeChildren(fRoot);
		Object primary= elements.length > 0 ? elements[0] : null;
		if (primary != null) {
			fTreeViewer.setSelection(new StructuredSelection(primary));
		}
		fTreeViewer.getControl().setFocus();
	}
	/**
	 *	Adds the passed listener to self's collection of clients
	 *	that listen for changes to element checked states
	 *
	 *	@param listener ICheckStateListener
	 */
	public void addCheckStateListener(ICheckStateListener listener) {
		fListeners.add(listener);
	}
	
	/**
	 * Adds the receiver and all of it's ancestors to the checkedStateStore if they
	 * are not already there.
	 */
	protected void addToHierarchyToCheckedStore(Object treeElement) {

		// if this tree element is already gray then its ancestors all are as well
		if (!fCheckedStateStore.contains(treeElement))
			fCheckedStateStore.add(treeElement);

		Object parent= fTreeContentProvider.getParent(treeElement);
		if (parent != null)
			addToHierarchyToCheckedStore(parent);
		fTreeViewer.setExpandedState(treeElement, true);
	}


	/**
	 *	Returns a boolean indicating whether all children of the passed tree element
	 *	are currently white-checked
	 *
	 *	@return boolean
	 *	@param treeElement java.lang.Object
	 */
	protected boolean areAllChildrenWhiteChecked(Object treeElement) {
		Object[] children= getTreeChildren(treeElement);
		for (int i= 0; i < children.length; ++i) {
			if (!fWhiteCheckedTreeItems.contains(children[i]))
				return false;
		}

		return true;
	}

	/**
	 *	Iterates through the passed elements which are being realized for the first
	 *	time and check each one in the tree viewer as appropriate
	 */
	protected void checkNewTreeElements(Object[] elements) {
		for (int i= 0; i < elements.length; ++i) {
			Object currentElement= elements[i];
			boolean checked= fCheckedStateStore.contains(currentElement);
			fTreeViewer.setChecked(currentElement, checked);
			fTreeViewer.setGrayed(
				currentElement,
				checked && !fWhiteCheckedTreeItems.contains(currentElement));
		}
	}
	/**
	*	An item was checked in one of self's two views.  Determine which
	*	view this occurred in and delegate appropriately
	*
	*	@param event CheckStateChangedEvent
	*/
	public void checkStateChanged(final CheckStateChangedEvent event) {

		//Potentially long operation - show a busy cursor
		BusyIndicator.showWhile(fTreeViewer.getControl().getDisplay(), new Runnable() {
			public void run() {
				if (event.getCheckable().equals(fTreeViewer))
					treeItemChecked(event.getElement(), event.getChecked());

				notifyCheckStateChangeListeners(event);
			}
		});
	}
	
	/**
	 * Sets an item's checked state as if the user had clicked on it
	 * @param treeElement
	 * @param state
	 */
	public void selectTreeCheck(Object treeElement, boolean state) {
		treeItemChecked(treeElement, state);
	}

	/**
	 * Returns a boolean indicating whether the passed tree element should be
	 * at LEAST gray-checked.  Note that this method does not consider whether
	 * it should be white-checked, so a specified tree item which should be
	 * white-checked will result in a <code>true</code> answer from this method.
	 * To determine whether a tree item should be white-checked use method
	 * #determineShouldBeWhiteChecked(Object).
	 *
	 * @param treeElement java.lang.Object
	 * @return boolean
	 * @see #determineShouldBeWhiteChecked(java.lang.Object)
	 */
	protected boolean determineShouldBeAtLeastGrayChecked(Object treeElement) {
		// if any children of treeElement are still gray-checked then treeElement
		// must remain gray-checked as well
		Object[] children= getTreeChildren(treeElement);
		for (int i= 0; i < children.length; ++i) {
			if (fCheckedStateStore.contains(children[i]))
				return true;
		}

		return false;
	}
	
	/**
	 * Returns a boolean indicating whether the passed tree item should be
	 * white-checked.
	 *
	 * @return boolean
	 * @param treeElement java.lang.Object
	 */
	protected boolean determineShouldBeWhiteChecked(Object treeElement) {
		return areAllChildrenWhiteChecked(treeElement) && fCheckedStateStore.contains(treeElement);
	}
	
	/**
	 *	Recursively adds appropriate tree elements to the collection of
	 *	known white-checked tree elements.
	 *
	 *	@param treeElement java.lang.Object
	 */
	protected void determineWhiteCheckedDescendents(Object treeElement) {
		// always go through all children first since their white-checked
		// statuses will be needed to determine the white-checked status for
		// this tree element
		Object[] children= getTreeChildren(treeElement);
		for (int i= 0; i < children.length; ++i)
			determineWhiteCheckedDescendents(children[i]);

		// now determine the white-checked status for this tree element
		if (determineShouldBeWhiteChecked(treeElement))
			setWhiteChecked(treeElement, true);
	}
	
	/**
	 * Causes the tree viewer to expand all its items
	 */
	public void expandAll() {
		fTreeViewer.expandAll();
	}
	
	/**
	 * Causes the tree viewer to collapse all its items
	 */
	public void collapseAll() {
		fTreeViewer.collapseAll();
	}

	/**
	 *	Answer a collection of all of the checked elements in the tree portion
	 *	of self
	 *
	 *	@return java.util.Vector
	 */
	public Set<Object> getAllCheckedTreeItems() {
		return new HashSet<Object>(fCheckedStateStore);
	}
	/**
	 *	Answers the number of elements that have been checked by the
	 *	user.
	 *
	 *	@return int
	 */
	public int getCheckedElementCount() {
		return fCheckedStateStore.size();
	}

	/**
	 * Gets the tree that displays the list for a folder
	 * 
	 * @return the tree used to show the folders
	 */
	public Tree getTree() {
		return fTreeViewer.getTree();
	}
	
	/**
	 * Adds the given filter to the tree viewer and
	 * triggers refiltering and resorting of the elements.
	 *
	 * @param filter a viewer filter
	 */
	public void addTreeFilter(ViewerFilter filter) {
		fTreeViewer.addFilter(filter);
	}

	/**
	 *	Logically gray-check all ancestors of treeItem by ensuring that they
	 *	appear in the checked table
	 */
	protected void grayCheckHierarchy(Object treeElement) {

		// if this tree element is already gray then its ancestors all are as well
		if (fCheckedStateStore.contains(treeElement))
			return; // no need to proceed upwards from here

		fCheckedStateStore.add(treeElement);
		if (determineShouldBeWhiteChecked(treeElement)) {
			setWhiteChecked(treeElement, true);
		}
		Object parent= fTreeContentProvider.getParent(treeElement);
		if (parent != null)
			grayCheckHierarchy(parent);
	}
	
	/**
	 *	Sets the initial checked state of the passed element to true,
	 *	as well as to all of its children and associated list elements
	 */
	public void initialCheckTreeItem(Object element) {
		treeItemChecked(element, true);
	}
	/**
	 *	Initializes this group's viewers after they have been laid out.
	 */
	protected void initialize() {
		fTreeViewer.setInput(fRoot);
	}

	/**
	 *	Notifies all checked state listeners that the passed element has had
	 *	its checked state changed to the passed state
	 */
	protected void notifyCheckStateChangeListeners(CheckStateChangedEvent event) {
		Iterator listenersEnum= fListeners.iterator();
		while (listenersEnum.hasNext())
			 ((ICheckStateListener) listenersEnum.next()).checkStateChanged(event);
	}

	/**
	 *	Removes the passed listener from self's collection of clients
	 *	that listen for changes to element checked states
	 *
	 *	@param listener ICheckStateListener
	 */
	public void removeCheckStateListener(ICheckStateListener listener) {
		fListeners.remove(listener);
	}
	
	/**
	 *	Handles the selection of an item in the tree viewer
	 *
	 *	@param event ISelection
	 */
	public void selectionChanged(final SelectionChangedEvent event) {
//		BusyIndicator.showWhile(getTree().getShell().getDisplay(), new Runnable() {
//			public void run() {
//				IStructuredSelection selection= (IStructuredSelection) event.getSelection();
//				Object selectedElement= selection.getFirstElement();
//				if (selectedElement == null) {
//					return;
//				} else {
//				}
//			}
//		});
	}

	/**
	 * Selects or deselect all of the elements in the tree depending on the value of the selection
	 * boolean. Be sure to update the displayed files as well.
	 */
	public void setAllSelections(final boolean selection) {

		//Potentially long operation - show a busy cursor
		BusyIndicator.showWhile(fTreeViewer.getControl().getDisplay(), new Runnable() {
			public void run() {
				setTreeChecked(fRoot, selection);
			}
		});
	}

	/**
	 * Sets the root of the widget to be new Root. Regenerate all of the tables and lists from this
	 * value.
	 * 
	 * @param newRoot 
	 */
	public void setRoot(Object newRoot) {
		this.fRoot= newRoot;
		initialize();
	}
	
	/**
	 *	Sets the checked state of the passed tree element appropriately, and
	 *	do so recursively to all of its child tree elements as well
	 */
	protected void setTreeChecked(Object treeElement, boolean state) {

		if (state) {
			fCheckedStateStore.add(treeElement);
		} else
			fCheckedStateStore.remove(treeElement);

		setWhiteChecked(treeElement, state);
		fTreeViewer.setChecked(treeElement, state);
		fTreeViewer.setGrayed(treeElement, false);

		// now logically check/uncheck all children as well
		Object[] children= getTreeChildren(treeElement);
		for (int i= 0; i < children.length; ++i) {
			setTreeChecked(children[i], state);
		}
	}
	/**
	 *	Sets the tree viewer's providers to those passed
	 *
	 *	@param contentProvider ITreeContentProvider
	 *	@param labelProvider ILabelProvider
	 */
	public void setTreeProviders(
		ITreeContentProvider contentProvider,
		ILabelProvider labelProvider) {
		fTreeViewer.setContentProvider(contentProvider);
		fTreeViewer.setLabelProvider(labelProvider);
	}
	/**
	 *	Sets the sorter that is to be applied to self's tree viewer
	 */
	public void setTreeSorter(ViewerSorter sorter) {
		fTreeViewer.setSorter(sorter);
	}
	/**
	 *	Adjusts the collection of references to white-checked tree elements appropriately.
	 *
	 *	@param treeElement java.lang.Object
	 *	@param isWhiteChecked boolean
	 */
	protected void setWhiteChecked(Object treeElement, boolean isWhiteChecked) {
		if (isWhiteChecked) {
			if (!fWhiteCheckedTreeItems.contains(treeElement))
				fWhiteCheckedTreeItems.add(treeElement);
		} else
			fWhiteCheckedTreeItems.remove(treeElement);
	}
	/**
	 *	Handle the collapsing of an element in a tree viewer
	 */
	public void treeCollapsed(TreeExpansionEvent event) {
		// We don't need to do anything with this
	}

	/**
	 *	Handles the expansionsion of an element in a tree viewer
	 */
	public void treeExpanded(TreeExpansionEvent event) {

		Object item= event.getElement();

		// First see if the children need to be given their checked state at all.  If they've
		// already been realized then this won't be necessary
		if (!fExpandedTreeNodes.contains(item)) {
			fExpandedTreeNodes.add(item);
			checkNewTreeElements(getTreeChildren(item));
		}
	}

	/**
	 *  Callback that's invoked when the checked status of an item in the tree
	 *  is changed by the user.
	 */
	protected void treeItemChecked(Object treeElement, boolean state) {

		// recursively adjust all child tree elements appropriately
		setTreeChecked(treeElement, state);

		Object parent= fTreeContentProvider.getParent(treeElement);
		if (parent == null)
			return;

		// now update upwards in the tree hierarchy 
		if (state)
			grayCheckHierarchy(parent);
		else
			ungrayCheckHierarchy(parent);

		updateHierarchy(treeElement);
	}
	/**
	 *	Logically un-gray-check all ancestors of treeItem iff appropriate.
	 */
	protected void ungrayCheckHierarchy(Object treeElement) {
		if (!determineShouldBeAtLeastGrayChecked(treeElement))
			fCheckedStateStore.remove(treeElement);

		Object parent= fTreeContentProvider.getParent(treeElement);
		if (parent != null)
			ungrayCheckHierarchy(parent);
	}
	/**
	 *	Sets the checked state of self and all ancestors appropriately
	 */
	protected void updateHierarchy(Object treeElement) {

		boolean whiteChecked= determineShouldBeWhiteChecked(treeElement);
		boolean shouldBeAtLeastGray= determineShouldBeAtLeastGrayChecked(treeElement);

		fTreeViewer.setChecked(treeElement, whiteChecked || shouldBeAtLeastGray);
		setWhiteChecked(treeElement, whiteChecked);
		if (whiteChecked)
			fTreeViewer.setGrayed(treeElement, false);
		else
			fTreeViewer.setGrayed(treeElement, shouldBeAtLeastGray);

		// proceed up the tree element hierarchy
		Object parent= fTreeContentProvider.getParent(treeElement);
		if (parent != null) {
			updateHierarchy(parent);
			if (whiteChecked || shouldBeAtLeastGray)
				fTreeViewer.setExpandedState(parent, true);
		}
	}
	
	/**
	 * Returns the result of running the given elements through the filters.
	 *
	 * @param elements the elements to filter
	 * @return only the elements which all filters accept
	 */
	protected Object[] filter(ViewerFilter[] filters, Object[] elements) {
		if (filters != null) {
			ArrayList<Object> filtered = new ArrayList<Object>(elements.length);
			for (int i = 0; i < elements.length; i++) {
				boolean add = true;
				for (int j = 0; j < filters.length; j++) {
					add = filters[j].select(fTreeViewer, null, elements[i]);
					if (!add)
						break;
				}
				if (add)
					filtered.add(elements[i]);
			}
			return filtered.toArray();
		}
		return elements;
	}

	protected Object[] getTreeChildren(Object element) {
		return filter(fTreeViewer.getFilters(), fTreeContentProvider.getChildren(element));
//		return fTreeContentProvider.getChildren(element);
	}

	public Set<Object> getWhiteCheckedTreeItems() {
		return new HashSet<Object>(fWhiteCheckedTreeItems);
	}
	
	/**
	 * Update the selections of the tree elements in items to reflect the new
	 * selections provided.
	 * 
	 * @param items with keys of Object (the tree element) and values of List (the selected
	 * list elements).
	 */
	public void updateSelections(final List items) {

		//Potentially long operation - show a busy cursor
		BusyIndicator.showWhile(fTreeViewer.getControl().getDisplay(), new Runnable() {
			public void run() {
				fExpandedTreeNodes.clear();
				handleUpdateSelection(items);
			}
		});
	}

	protected void handleUpdateSelection(List items) {
		Iterator iterator= items.iterator();

		//Update the store before the hierarchy to prevent updating parents before all of the children are done
		while (iterator.hasNext()) {
			Object item= iterator.next();
			//Replace the items in the checked state store with those from the supplied items
			if (!fCheckedStateStore.contains(item))
				fCheckedStateStore.add(item);
			// proceed up the tree element hierarchy
			Object parent= fTreeContentProvider.getParent(item);
			if (parent != null) {
				addToHierarchyToCheckedStore(parent);
			}
		}

		//Now update hierarchies
		iterator= items.iterator();

		while (iterator.hasNext()) {
			Object item= iterator.next();
			updateHierarchy(item);
		}
	}		
	
	/**
	 * Checks if an element is grey checked.
	 */
	public boolean isTreeItemGreyChecked(Object object) {
		return fTreeViewer.getGrayed(object);	
	}	

	/**
	 * For a given element, expand its chidren to a level.
	 */	
	public void expandTreeToLevel(Object object, int level) {
		fTreeViewer.expandToLevel(object, level);	
	}
	/**
	 * @param selection
	 */
	public void setTreeSelection(ISelection selection) {
		fTreeViewer.setSelection(selection);
	}
	
	/**
	 * Refreshes the content of the viewer.
	 */
	public void refresh() {
		fExpandedTreeNodes.clear();
		fTreeViewer.refresh();
	}

}