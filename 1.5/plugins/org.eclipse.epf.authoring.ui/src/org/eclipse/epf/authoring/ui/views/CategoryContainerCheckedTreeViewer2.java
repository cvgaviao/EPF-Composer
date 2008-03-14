package org.eclipse.epf.authoring.ui.views;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;

/**
 * Viewer for showing categories.  Accepts a MethodConfiguration so it will 
 * only show items in the config.
 * 
 * Accepts a Text widget for displaying the selected item's description.
 * 
 * @author Jeff Hardy
 *
 */
public class CategoryContainerCheckedTreeViewer2 extends
		MethodContainerCheckedTreeViewer2 {
	
	protected Text textWidget;
	
	protected MethodConfiguration config;

    /**
     * Constructor for CategoryContainerCheckedTreeViewer2.
     * @see CheckboxTreeViewer#CheckboxTreeViewer(Composite)
     */
    public CategoryContainerCheckedTreeViewer2(Composite parent) {
        super(parent);
        initViewer();
    }

    /**
     * Constructor for CategoryContainerCheckedTreeViewer2.
     * @see CheckboxTreeViewer#CheckboxTreeViewer(Composite,int)
     */
    public CategoryContainerCheckedTreeViewer2(Composite parent, int style) {
        super(parent, style);
        initViewer();
    }

    /**
     * Constructor for CategoryContainerCheckedTreeViewer2.
     * @see CheckboxTreeViewer#CheckboxTreeViewer(Tree)
     */
    public CategoryContainerCheckedTreeViewer2(Tree tree) {
        super(tree);
        initViewer();
    }

	public Text getTextWidget() {
		return textWidget;
	}

	public void setTextWidget(Text textWidget) {
		this.textWidget = textWidget;
	}

	public MethodConfiguration getConfig() {
		return config;
	}

	public void setConfig(MethodConfiguration config) {
		this.config = config;
	}

	public Set<ContentCategory> getCheckedContentCategories() {
		Set<ContentCategory> result = new HashSet<ContentCategory>();
		List<Object> checkedItems = Arrays.asList(getCheckedElements());
		for (Object item : checkedItems) {
			item = TngUtil.unwrap(item);
			if (item instanceof ContentCategory) {
				if (config.getMethodPluginSelection().contains(LibraryUtil.getMethodPlugin((ContentCategory)item))) {
					result.add((ContentCategory)item);
				}
			}
		}
		return result;
	}
    
}
