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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.actions.LibraryViewSimpleAction;
import org.eclipse.epf.authoring.ui.actions.UnassignAction;
import org.eclipse.epf.authoring.ui.forms.CustomCategoryAssignPage;
import org.eclipse.epf.common.ui.util.MsgDialog;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.ui.UserInteractionHelper;
import org.eclipse.epf.library.edit.util.IRunnableWithProgress;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.services.LibraryModificationHelper;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.ProgressMonitorPart;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
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

	private ProgressMonitorPart progressMonitorPart;

	private Cursor waitCursor;
	
	private boolean lockedUI = false;

	public static AssignDialog newAssignDialog(Shell parentShell, Collection elements) {
		return new AssignDialog(parentShell, elements);
	}
	
	public static AssignDialog newReassignDialog(Shell parentShell,
			Collection elements, MethodElement parentElement) {
		return new ReassignDialog(parentShell, elements, parentElement);
	}
	
	public static AssignDialog newDeepCopyDialog(Shell parentShell, Collection elements) {
		return new CustomCategoryDeepCopyDialog(parentShell, elements);
	}
	
	protected AssignDialog(Shell parentShell, Collection elementsToAssign) {
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

		GridLayout pmlayout = new GridLayout();
		pmlayout.numColumns = 1;
		progressMonitorPart = createProgressMonitorPart(composite, pmlayout);
		progressMonitorPart
				.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		progressMonitorPart.setVisible(false);

		waitCursor = parent.getShell().getDisplay().getSystemCursor(
				SWT.CURSOR_WAIT);
		
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
		
		IRunnableWithProgress runnable = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor)
					throws InvocationTargetException, InterruptedException {
				monitor.subTask(AuthoringUIResources.assignAction_text);
				try {
					Collection<Resource> resouresToSave = doWorkBeforeSave();
					LibraryViewSimpleAction.save(resouresToSave);
				} finally {
					assigning = false;
				}
			}

		};
		
		final Shell shell = getShell();
		shell.setCursor(waitCursor);

		getButton(IDialogConstants.OK_ID).setEnabled(false);
		getButton(IDialogConstants.CANCEL_ID).setEnabled(false);
		treeViewer.getControl().setEnabled(false);

		progressMonitorPart.setVisible(true);
		IStatus stat = null;
		try {
			stat = UserInteractionHelper.getUIHelper().runInModalContext(runnable, true, progressMonitorPart, shell);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		} finally {
			progressMonitorPart.done();
		}
		if(stat != null && !stat.isOK()) {
			return false;
		}

		return true;
	}

	protected Collection<Resource> doWorkBeforeSave() {
		LibraryModificationHelper helper = new LibraryModificationHelper();
		CustomCategory category = (CustomCategory) destination;
		
		CustomCategoryAssignPage.addItemsToModel1(elements, category, usedCategories,
				helper.getActionManager(), CustomCategoryAssignPage.getAncestors(category));

		Collection<Resource> resouresToSave = new HashSet<Resource>();
		resouresToSave.add(category.eResource());
		return resouresToSave;
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
	

	protected ProgressMonitorPart createProgressMonitorPart(
			Composite composite, GridLayout pmlayout) {
		return new ProgressMonitorPart(composite, pmlayout, SWT.DEFAULT) {
			String currentTask = null;

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.wizard.ProgressMonitorPart#setBlocked(org.eclipse.core.runtime.IStatus)
			 */
			public void setBlocked(IStatus reason) {
				super.setBlocked(reason);
				if (!lockedUI)// Do not show blocked if we are locking the UI
					getBlockedHandler().showBlocked(getShell(), this, reason,
							currentTask);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.wizard.ProgressMonitorPart#clearBlocked()
			 */
			public void clearBlocked() {
				super.clearBlocked();
				if (!lockedUI)// Do not vlear if we never set it
					getBlockedHandler().clearBlocked();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.wizard.ProgressMonitorPart#beginTask(java.lang.String,
			 *      int)
			 */
			public void beginTask(String name, int totalWork) {
				super.beginTask(name, totalWork);
				currentTask = name;
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.wizard.ProgressMonitorPart#setTaskName(java.lang.String)
			 */
			public void setTaskName(String name) {
				super.setTaskName(name);
				currentTask = name;
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.wizard.ProgressMonitorPart#subTask(java.lang.String)
			 */
			public void subTask(String name) {
				super.subTask(name);
				// If we haven't got anything yet use this value for more
				// context
				if (currentTask == null)
					currentTask = name;
			}
		};
	}
	
	protected ArrayList getElements() {
		return elements;
	}
	
	protected Object getDestination() {
		return destination;
	}

	private static class ReassignDialog extends AssignDialog {
		
		private CustomCategory parentElement;
		
		protected ReassignDialog(Shell parentShell, 
				Collection elements, MethodElement parentElement) {
			super(parentShell, elements);
			this.parentElement = (CustomCategory) parentElement;
		}
		
		protected Collection<Resource> doWorkBeforeSave() {
			Collection<Resource> resouresToSave = super.doWorkBeforeSave();
			resouresToSave.add(parentElement.eResource());			
			UnassignAction.unassign(getElements().get(0), parentElement, new ArrayList());
			return resouresToSave;
		}
		
		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText(AuthoringUIResources.AssignDialog_reassign_text); 
		}
		
	}
	
	private static class CustomCategoryDeepCopyDialog extends AssignDialog {
		private ContentPackage hiddenPkg;
		
		protected CustomCategoryDeepCopyDialog(Shell parentShell, 
				Collection elements) {
			super(parentShell, elements);
		}
		
		protected Collection<Resource> doWorkBeforeSave() {
			if (getElements() == null
					|| !(getElements().get(0) instanceof CustomCategory)
					|| !(getDestination() instanceof CustomCategory)) {
				return null;
			}
					
			CustomCategory source = (CustomCategory) getElements().get(0);
			CustomCategory targetParent = (CustomCategory) getDestination();
			hiddenPkg = (ContentPackage) targetParent.eContainer();
			
			CustomCategory copy = (CustomCategory) deepCopy(source);
			TngUtil.setDefaultName(targetParent, copy, copy.getName());
			
			//ITextReferenceReplacer txtRefReplacer = ExtensionManager.getTextReferenceReplacer();
			
			hiddenPkg.getContentElements().add(copy);
			targetParent.getCategorizedElements().add(copy);
			
			Collection<Resource> resouresToSave = new ArrayList();				
			resouresToSave.add(targetParent.eResource());
			
			return resouresToSave;
		}
		
		private EObject deepCopy(EObject source) {			
			EObject copy = UmaFactory.eINSTANCE.create(source.eClass());
		
			List features = source.eClass().getEAllStructuralFeatures();
			for (int i = 0; i < features.size(); i++) {
				EStructuralFeature feature = (EStructuralFeature) features.get(i);				
				//System.out.println("LD> feature: " + feature.getName());
				copyFeatureValue(source, copy, feature);
			}
			return copy;
		}
		
		private void copyFeatureValue(EObject sourceObj, EObject copiedObj, EStructuralFeature feature) {
				
			if (feature instanceof EAttribute) {
				copyAttributeFeatureValue(sourceObj, copiedObj, (EAttribute) feature);				
			} else if (feature instanceof EReference) {
				copyReferenceFeatureValue(sourceObj, copiedObj, (EReference) feature);
			}
			
		}
		
		private void copyAttributeFeatureValue(EObject sourceObj,
				EObject copiedObj, EAttribute feature) {
			Object sourceValue = sourceObj.eGet(feature);
			if (sourceValue == null) {
				return;
			}
			Object copiedValue = sourceValue;

			if (sourceObj instanceof CustomCategory) {
				if (feature == UmaPackage.eINSTANCE.getMethodElement_Guid()) {
					copiedValue = EcoreUtil.generateUUID();
				}
				
				//MigrationUtil a;
			}

			copiedObj.eSet(feature, copiedValue);
		}

		private void copyReferenceFeatureValue(EObject sourceObj, EObject copiedObj, EReference feature) {
			Object sourceValue = sourceObj.eGet(feature);
			if (sourceValue == null) {
				return;
			}
			Object copiedValue = sourceValue;

			if (feature.isContainment()) {
				if (feature.isMany()) {
					List<EObject> sourceList = (List<EObject>) sourceValue;
					List<EObject> copiedList = (List<EObject>) copiedObj
							.eGet(feature);
					for (EObject sobj : sourceList) {
						EObject cobj = deepCopy(sobj);
						copiedList.add(cobj);
					}
					return;
				}
				copiedValue = deepCopy((EObject) sourceValue);

			} else if (feature.isMany()) {
				List sourceList = (List) sourceValue;
				List copiedList = (List) copiedObj.eGet(feature);
				for (Object sobj : sourceList) {
					Object cobj = sobj;
					if (sobj instanceof CustomCategory) {
						cobj = (CustomCategory) deepCopy((CustomCategory) sobj);
						CustomCategory ccobj = (CustomCategory) cobj;
						TngUtil.setDefaultName((CustomCategory) copiedObj, ccobj, ccobj.getName());
						hiddenPkg.getContentElements().add(ccobj);
					}
					copiedList.add(cobj);
				}
				return;
			}

			copiedObj.eSet(feature, copiedValue);
		}

		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText(AuthoringUIResources.deepCopy_text); 
		}
		
	}





}
