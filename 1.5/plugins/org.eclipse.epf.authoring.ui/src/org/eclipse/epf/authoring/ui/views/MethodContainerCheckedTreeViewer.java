package org.eclipse.epf.authoring.ui.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.dialogs.ContainerCheckedTreeViewer;

/**
 * ContainerCheckedTreeViewer class that supports MethodElements.
 * Specifically, has changes to support FeatureValueWrapperItemProviders.
 * 
 * Also, expands the tree for checked items when they are set via
 * setCheckedElements(..) iff expandWhenChecking is true.
 * @author Jeff Hardy
 *
 */
public class MethodContainerCheckedTreeViewer extends
		ContainerCheckedTreeViewer {
	
	protected boolean expandWhenChecking = false;

	protected ListenerList childrenCheckStateListeners = new ListenerList();

    /**
     * Constructor for MethodContainerCheckedTreeViewer.
     * @see CheckboxTreeViewer#CheckboxTreeViewer(Composite)
     */
    public MethodContainerCheckedTreeViewer(Composite parent) {
        super(parent);
        initViewer();
    }

    /**
     * Constructor for MethodContainerCheckedTreeViewer.
     * @see CheckboxTreeViewer#CheckboxTreeViewer(Composite,int)
     */
    public MethodContainerCheckedTreeViewer(Composite parent, int style) {
        super(parent, style);
        initViewer();
    }

    /**
     * Constructor for MethodContainerCheckedTreeViewer.
     * @see CheckboxTreeViewer#CheckboxTreeViewer(Tree)
     */
    public MethodContainerCheckedTreeViewer(Tree tree) {
        super(tree);
        initViewer();
    }
    
    protected void initViewer() {
        setUseHashlookup(true);
        addCheckStateListener(new ICheckStateListener() {
            public void checkStateChanged(CheckStateChangedEvent event) {
                doCheckStateChanged(event.getElement());
            }
        });
        addTreeListener(new ITreeViewerListener() {
            public void treeCollapsed(TreeExpansionEvent event) {
            }

            public void treeExpanded(TreeExpansionEvent event) {
                Widget item = findItem(event.getElement());
                if (item instanceof TreeItem) {
                    initializeItem((TreeItem) item);
                }
            }
        });
    }
    
    /**
     * The item has expanded. Updates the checked state of its children. 
     */
    protected void initializeItem(TreeItem item) {
        if (item.getChecked() && !item.getGrayed()) {
            updateChildrenItems(item);
        }
    }
    
    /**
     * Updates the check state of all created children
     */
    protected void updateChildrenItems(TreeItem parent) {
        Item[] children = getChildren(parent);
        boolean state = parent.getChecked();
        for (int i = 0; i < children.length; i++) {
            TreeItem curr = (TreeItem) children[i];
            if (curr.getData() != null
                    && ((curr.getChecked() != state) || curr.getGrayed())) {
                curr.setChecked(state);
                curr.setGrayed(false);
                notifyChildrenCheckStateListeners(new CheckStateChangedEvent(this, curr.getData(),
                		state));                
                updateChildrenItems(curr);
            }
        }
    }
    
	/**
	 * Update element after a checkstate change.
	 * @param element
	 */
    public void doCheckStateChanged(Object element) {
        Widget item = findItem(element);
        if (item instanceof TreeItem) {
            TreeItem treeItem = (TreeItem) item;
            treeItem.setGrayed(false);
            updateChildrenItems(treeItem);
            updateParentItems(treeItem.getParentItem());
        }
    }
    
    public void updateParents(Object element) {
        Widget item = findItem(element);
        if (item instanceof TreeItem) {
            TreeItem treeItem = (TreeItem) item;
            updateParentItems(treeItem.getParentItem());
        }
    }

    /**
     * Updates the check / gray state of all parent items
     */
    protected void updateParentItems(TreeItem item) {
        if (item != null) {
            Item[] children = getChildren(item);
            boolean containsChecked = false;
            boolean containsUnchecked = false;
            for (int i = 0; i < children.length; i++) {
                TreeItem curr = (TreeItem) children[i];
                containsChecked |= curr.getChecked();
                containsUnchecked |= (!curr.getChecked() || curr.getGrayed());
            }
            item.setChecked(containsChecked);
            item.setGrayed(containsChecked && containsUnchecked);
            if (expandWhenChecking && item.getChecked()) {
            	item.setExpanded(true);
            }
            updateParentItems(item.getParentItem());
        }
    }
    
    
    /* (non-Javadoc)
     * @see org.eclipse.jface.viewers.CheckboxTreeViewer#setCheckedElements(java.lang.Object[])
     */
    public void setCheckedElements(Object[] elements) {
    	// fix elements list to use our wrappers
    	List<Object> treeElements = new ArrayList<Object>();
    	AdapterFactory adapterFactory = null;
    	if (getContentProvider() instanceof AdapterFactoryContentProvider) {
    		adapterFactory = ((AdapterFactoryContentProvider)getContentProvider()).getAdapterFactory();
    	}
    	for (int i = 0;i < elements.length; i++) {
    		Object element = elements[i];
    		if (element instanceof CustomCategory && adapterFactory != null) {
    			treeElements.add(TngUtil.getFeatureValueWrapperItemProviderForCC(adapterFactory, (CustomCategory)element));
    		} else {
    			treeElements.add(element);
    		}
    	}
        super.setCheckedElements(treeElements.toArray());
        for (Object element : treeElements) {
        	doCheckStateChanged(element);
        }
    }
    
    /**
     * Returns a list of top-most checked elements in this viewer's tree, 
     * including currently hidden ones that are marked as
     * checked but are under a collapsed ancestor.
     *
     * Only returns white-checked items, not grayed items
     *
     * @return the array of checked elements
     *
     * @see #setCheckedElements
     */
    public Object[] getTopCheckedElements() {
        ArrayList v = new ArrayList();
        Control tree = getControl();
        internalCollectTopChecked(v, tree);
        return v.toArray();
    }
    
    
    /**
     * Gathers the top-most checked states of the given widget and its
     * descendents, following a pre-order traversal of the tree.
     *
     * @param result a writable list of elements (element type: <code>Object</code>)
     * @param widget the widget
     */
    private void internalCollectTopChecked(List result, Widget widget) {
        Item[] items = getChildren(widget);
        for (int i = 0; i < items.length; i++) {
            Item item = items[i];
            if (item instanceof TreeItem && ((TreeItem) item).getChecked() && !((TreeItem) item).getGrayed()) {
                Object data = item.getData();
                if (data != null) {
					result.add(data);
				}
            }
            if (item instanceof TreeItem && (!((TreeItem) item).getChecked() || ((TreeItem) item).getGrayed())) {
            	internalCollectTopChecked(result, item);
            }
        }
    }


	public boolean isExpandWhenChecking() {
		return expandWhenChecking;
	}

	public void setExpandWhenChecking(boolean expandWhenChecking) {
		this.expandWhenChecking = expandWhenChecking;
	}
	
	public void addChildrenCheckStateListener(ICheckStateListener listener) {
		childrenCheckStateListeners.add(listener);
	}
	
	public void removeChildrenCheckStateListener(ICheckStateListener listener) {
		childrenCheckStateListeners.remove(listener);
	}
	
	protected void notifyChildrenCheckStateListeners(final CheckStateChangedEvent event) {
		for (final Object listener : childrenCheckStateListeners.getListeners()) {
            SafeRunner.run(new SafeRunnable() {
                public void run() {
                	((ICheckStateListener)listener).checkStateChanged(event);
                }
            });
		}
	}

}
