/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

/**
 * @author achen
 */
package org.eclipse.epf.authoring.ui.dialogs;

import java.util.List;

import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;


public class PluginListDialog extends Dialog {
	private boolean readOnly;
	private List methodPlugins;
	
	private TreeViewer viewer;
	private ComposedAdapterFactory adapterFactory = TngAdapterFactory.INSTANCE.createLibraryComposedAdapterFactory();
	private ITreeContentProvider pluginsContentProvider = new PluginListContentProvider();
	private AdapterFactoryLabelProvider pluginsLabelProvider = new AdapterFactoryLabelProvider(adapterFactory);
	private Button addBtn;
	private Button removeBtn;
	
	public PluginListDialog(Shell shell, boolean readOnly, List methodPlugins) {
		super(shell);
		this.readOnly = readOnly;
		this.methodPlugins = methodPlugins;
	}
	
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(AuthoringUIResources.PluginListDialog_title);
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		GridLayout layout = (GridLayout) composite.getLayout();
		layout.numColumns = 2;
		
		Composite viewComp = new Composite(composite, SWT.NONE);
		viewComp.setLayoutData(new GridData(GridData.FILL_BOTH));
		viewComp.setLayout(new GridLayout());
		viewer = new TreeViewer(viewComp, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		viewer.setLabelProvider(pluginsLabelProvider);
		viewer.setContentProvider(pluginsContentProvider);
		GridData viewerGd = new GridData(GridData.FILL_BOTH);
		viewerGd.heightHint = 250;
		viewerGd.widthHint = 200;
		viewer.getTree().setLayoutData(viewerGd);
				
		Composite btnComp = new Composite(composite, SWT.NONE);
		btnComp.setLayoutData(new GridData(GridData.FILL_BOTH));
		btnComp.setLayout(new GridLayout());
		addBtn = new Button(btnComp, SWT.PUSH);
		addBtn.setText(AuthoringUIResources.PluginListDialog_button_add);
		addBtn.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		removeBtn = new Button(btnComp, SWT.PUSH);
		removeBtn.setText(AuthoringUIResources.PluginListDialog_button_remove);
		removeBtn.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		updateControls();
		
		return composite;
	}
	
	protected void updateControls() {
		if (readOnly) {
			viewer.setInput(methodPlugins);
			viewer.refresh();
			addBtn.setEnabled(false);
			removeBtn.setEnabled(false);
		}
	}
	
	private class PluginListContentProvider implements ITreeContentProvider {
		public Object[] getChildren(Object parentElement) {
			return null;
		}

		public Object getParent(Object element) {
			return null;
		}

		public boolean hasChildren(Object element) {
			return false;
		}

		public Object[] getElements(Object inputElement) {
			if (inputElement instanceof List) {
				return ((List)inputElement).toArray();
			}
			
			return new Object[0];
		}

		public void dispose() {			
		}
		
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {			
		}	
	}

}
