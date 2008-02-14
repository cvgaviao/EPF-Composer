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
package org.eclipse.epf.authoring.ui.dialogs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.forms.CustomCategoryAssignPage;
import org.eclipse.epf.common.ui.util.MsgDialog;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.ui.UserInteractionHelper;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.services.LibraryModificationHelper;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;


/**
 * Dialog for assiging MethodElement within MethodLibrary.
 * 
 * @author Weiping Lu
 * @since 1.5
 */
public class AssignDialog extends Dialog implements ISelectionChangedListener {

	ArrayList usedCategories = new ArrayList();
	
	private TreeViewer treeViewer;

	private ArrayList elements;

	private Object destination;
	
	private boolean assigning = false;

	public AssignDialog(Shell parentShell, Collection elementsToAssign) {
		super(parentShell);

		// filter out the predefined elements to prevent them from getting moved
		//
		elements = new ArrayList();
		for (Iterator iter = elementsToAssign.iterator(); iter.hasNext();) {
			Object element = iter.next();
			Object e = TngUtil.unwrap(element);
			if (e instanceof MethodElement
					&& TngUtil.isPredefined((MethodElement) e)) {
				continue;
			}
			elements.add(element);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);

		GridLayout layout = (GridLayout) composite.getLayout();
		layout.marginWidth = 10;
		layout.marginHeight = 10;

		Label label = new Label(composite, SWT.NONE);
		label.setText(AuthoringUIResources.MoveDialog_destination_text); 
		GridData layoutData = new GridData(SWT.BEGINNING);
		label.setLayoutData(layoutData);

		treeViewer = new TreeViewer(composite, SWT.SINGLE | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		AdapterFactory adapterFactory = TngAdapterFactory.INSTANCE
				.getNavigatorView_ComposedAdapterFactory();
		treeViewer.setLabelProvider(new AdapterFactoryLabelProvider(
				adapterFactory));
		treeViewer.setContentProvider(new AdapterFactoryContentProvider(
				adapterFactory));
		treeViewer.addSelectionChangedListener(this);
		// treeViewer.addDoubleClickListener(this);

		GridData spec = new GridData(GridData.FILL_BOTH);
		{
			spec.widthHint = 300;
			spec.heightHint = 300;
			treeViewer.getControl().setLayoutData(spec);
		}

		treeViewer.setInput(LibraryService.getInstance().getCurrentMethodLibrary());

		return composite;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(AuthoringUIResources.AssignDialog_assign_text); 
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
	 */
	public void selectionChanged(SelectionChangedEvent event) {
		destination = TngUtil.unwrap(((IStructuredSelection) event
				.getSelection()).getFirstElement());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	protected void okPressed() {
		if (doAssign()) {
			super.okPressed();
		}
	}

	private boolean isValidDestination() {
		// preventing moving elements to category
		//
		if (destination instanceof CustomCategory) {
			return true;
		}

		return false;
	}

	/**
	 * @return
	 */
	private boolean doAssign() {
		if (destination == null) {
			String title = AuthoringUIResources.errorDialog_title; 
			String problem = AuthoringUIResources.MoveDialog_nomove_destination_problem_msg; 
			String msg = AuthoringUIResources.MoveDialog_selectdestination_text; 
			MsgDialog dialog = AuthoringUIPlugin.getDefault().getMsgDialog();
			dialog.displayError(title, problem, msg); 
			return false;
		}
		if (!isValidDestination()) {
			String title = AuthoringUIResources.errorDialog_title; 
			String problem = AuthoringUIResources.MoveDialog_invalid_move_destination_problem_msg; 
			String msg = AuthoringUIResources.MoveDialog_validdestination_text; 
			MsgDialog dialog = AuthoringUIPlugin.getDefault().getMsgDialog();
			dialog.displayError(title, problem, msg); 
			return false;
		} else {
			IStatus status = UserInteractionHelper.checkModify(
					(EObject) destination, getShell());
			if (!status.isOK()) {
				String title = AuthoringUIResources.errorDialog_title; 
				String msg = AuthoringUIResources.MoveDialog_cannotModifyDestination; 
				MsgDialog dialog = AuthoringUIPlugin.getDefault()
						.getMsgDialog();
				dialog.displayError(title, msg, status); 
				return false;
			}
		}

		assigning = true;
		
		try {
			LibraryModificationHelper helper = new LibraryModificationHelper();
			CustomCategory category = (CustomCategory) destination;
			
			CustomCategoryAssignPage.addItemsToModel1(elements, category, usedCategories,
					helper.getActionManager(), CustomCategoryAssignPage.getAncestors(category));
		} finally {
			assigning = false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#close()
	 */
	public boolean close() {
		if (assigning)
			return false;
		return super.close();
	}

}
