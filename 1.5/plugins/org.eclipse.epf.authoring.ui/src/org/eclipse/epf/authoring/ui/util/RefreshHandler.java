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
package org.eclipse.epf.authoring.ui.util;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.ui.viewer.IViewerProvider;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditorInput;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.persistence.MultiFileResourceSetImpl;
import org.eclipse.epf.persistence.refresh.IRefreshHandler;
import org.eclipse.epf.persistence.refresh.RefreshJob;
import org.eclipse.epf.persistence.util.LibrarySchedulingRule;
import org.eclipse.epf.persistence.util.PersistenceUtil;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.part.ViewPart;

/**
 * Notify changes in resources to refresh
 * 
 * @author Phong Nguyen Le
 * @since 1.0
 */
public class RefreshHandler implements IRefreshHandler {
	
	private ViewPart view;


	public RefreshHandler(ViewPart view) {
		this.view = view;
	}
	
	private Control getControl() {
		if(view instanceof IViewerProvider) {
			Viewer viewer = ((IViewerProvider)view).getViewer();
			if(viewer != null) {
				return viewer.getControl();
			}
		}
		return null;
	}

	public void refresh(IProgressMonitor monitor) {
		Control ctrl = getControl();
		if (ctrl == null || ctrl.isDisposed())
			return;

		if (ctrl.getDisplay().getThread() == Thread.currentThread()) {
			doRefresh(view.getSite().getShell());
		} else {
			ctrl.getDisplay().syncExec(new Runnable() {

				public void run() {
					doRefresh(null);
				}

			});
		}
	}
	
	private void doRefresh(Shell shell) {
		final boolean refreshViews = !RefreshJob.getInstance()
				.getReloadedBeforeRefreshResources().isEmpty()
				|| !RefreshJob.getInstance().getAddedResources().isEmpty();
		ArrayList<Resource> removedResources = new ArrayList<Resource>(RefreshJob.getInstance()
				.getRemovedResources());
		ArrayList<Resource> changedResources = new ArrayList<Resource>(RefreshJob.getInstance()
				.getChangedResources());
		ArrayList<IResource> addedWsResources = new ArrayList<IResource>(RefreshJob.getInstance()
				.getAddedWorkspaceResources());

		if (!removedResources.isEmpty() || !changedResources.isEmpty()
				|| !addedWsResources.isEmpty() || refreshViews) {
			blockingRefresh(removedResources, changedResources,
					addedWsResources, shell);
		}

		if (!removedResources.isEmpty()) {
			RefreshJob.getInstance().getRemovedResources().removeAll(
					removedResources);
		}
		if (!changedResources.isEmpty()) {
			RefreshJob.getInstance().getChangedResources().removeAll(
					changedResources);
		}
		if (!addedWsResources.isEmpty()) {
			RefreshJob.getInstance().getAddedWorkspaceResources().removeAll(
					addedWsResources);
		}
		if (refreshViews) {
			RefreshJob.getInstance().getReloadedBeforeRefreshResources()
					.clear();
			RefreshJob.getInstance().getAddedResources().clear();
		}
	}
	
	private void blockingRefresh(final ArrayList<Resource> removedResources,
			final ArrayList<Resource> changedResources,
			final Collection<IResource> addedWorkspaceResources,
			Shell shell) {
		final IRunnableWithProgress runnable = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor)
					throws InvocationTargetException, InterruptedException {
				monitor.beginTask("", IProgressMonitor.UNKNOWN); //$NON-NLS-1$
				monitor
						.subTask(AuthoringUIResources._UI_RefreshViewer_menu_item);
				monitor.worked(1);
				try {
					// monitor.worked(1);
					doRefresh(removedResources, changedResources,
							addedWorkspaceResources);
				} finally {
					monitor.done();
				}
			}

		};

		IRunnableContext context = new ProgressMonitorDialog(shell);
		try {
			view.getSite().getWorkbenchWindow().getWorkbench().getProgressService()
					.runInUI(
							context,
							runnable,
							new LibrarySchedulingRule(LibraryService
									.getInstance().getCurrentMethodLibrary()));
		} catch (Exception e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
			String title = AuthoringUIResources.ProcessEditor_refreshErrorTitle;
			AuthoringUIPlugin.getDefault().getMsgDialog().displayError(title,
					e.toString(), e);
		}
	}

	
	/**
	 * Must be synchronized to avoid stepping on each other in reloading
	 * resources/refreshing UI
	 * 
	 * @param removedResources
	 * @param changedResources
	 * @param addedWorkspaceResources
	 *            collection of IResource objects that are just newly added to
	 *            the library
	 * @param refreshViews
	 */
	private synchronized void doRefresh(Collection<Resource> removedResources,
			Collection<Resource> changedResources, Collection<IResource> addedWorkspaceResources) {
		HashSet<IEditorPart> editorsToRefresh = new HashSet<IEditorPart>();
		if (!removedResources.isEmpty()) {
			handleRemovedResources(removedResources, editorsToRefresh);
		}
		if (!changedResources.isEmpty()) {
			handleChangedResources(changedResources, editorsToRefresh);
		}
		if (addedWorkspaceResources != null
				&& !addedWorkspaceResources.isEmpty()) {
			ILibraryManager mgr = LibraryService.getInstance()
					.getCurrentLibraryManager();
			if (mgr != null) {
				ResourceSet resourceSet = mgr.getEditingDomain()
						.getResourceSet();
				if (resourceSet instanceof MultiFileResourceSetImpl) {
					((MultiFileResourceSetImpl) resourceSet)
							.loadNewResources(addedWorkspaceResources);
					((MultiFileResourceSetImpl) resourceSet).getUnresolvedProxyMarkerManager().validateAllMarkers();
				}
			}
		}
	}
	
	private Collection<Resource> handleChangedResources(Collection<Resource> changedResources,
			Collection<IEditorPart> editorsToRefresh) {
		return handleChangedResources(changedResources, null, editorsToRefresh);
	}

	/**
	 * 
	 * @param removedResources
	 * @param editorsToRefresh
	 * @return resources that have been unloaded
	 */
	private Collection<Resource> handleRemovedResources(Collection<Resource> removedResources,
			Collection<IEditorPart> editorsToRefresh) {
		IWorkbenchPage workbenchPage = view.getSite().getPage();
		IEditorReference[] editorReferences = workbenchPage
				.getEditorReferences();
		ArrayList<IEditorPart> dirtyEditorsWithConflict = new ArrayList<IEditorPart>();
		ArrayList<Resource> removedResourceList = new ArrayList<Resource>(removedResources);
		if (editorsToRefresh == null) {
			editorsToRefresh = new ArrayList<IEditorPart>();
		}
		// find all editor with dirty conflict
		//
		for (int i = 0; i < editorReferences.length; i++) {
			IEditorReference reference = editorReferences[i];
			IEditorPart editor = reference.getEditor(true);
			if (editor instanceof MethodElementEditor && editor.isDirty()) {
				MethodElementEditorInput input = (MethodElementEditorInput) editor
						.getEditorInput();
				Resource resource = input.getMethodElement() != null ? input
						.getMethodElement().eResource() : null;
				if (!removedResources.contains(resource)) {
					Collection<Resource> usedResources = ((MethodElementEditor) editor)
							.getUsedResources();
					check_resource: for (int j = 0; j < removedResourceList
							.size(); j++) {
						resource = (Resource) removedResourceList.get(j);
						if (usedResources.contains(resource)) {
							dirtyEditorsWithConflict.add(editor);
							break check_resource;
						}
					}
				} else {
					editorsToRefresh.add(editor);
				}
			}
		}

		if (!dirtyEditorsWithConflict.isEmpty()) {
			Object[] selected = selectDirtyEditors(dirtyEditorsWithConflict);
			for (int i = 0; i < selected.length; i++) {
				editorsToRefresh.add((IEditorPart) selected[i]);
			}
		}

		// Unload the removed resources.
		PersistenceUtil.unload(removedResources);

		for (int i = 0; i < editorReferences.length; i++) {
			IEditorReference reference = editorReferences[i];
			IEditorPart editor = reference.getEditor(true);
			if (editor instanceof MethodElementEditor && !editor.isDirty()) {
				Collection<Resource> usedResources = ((MethodElementEditor) editor)
						.getUsedResources();
				check_resource: for (int j = 0; j < removedResourceList.size(); j++) {
					Resource resource = (Resource) removedResourceList.get(j);
					if (usedResources.contains(resource)) {
						editorsToRefresh.add(editor);
						break check_resource;
					}
				}
			}
		}

		return removedResources;
	}


	/**
	 * Reload the changed resources
	 * 
	 * @param changedResources
	 * @param editorsNotToRefresh
	 * @param editorsToRefresh
	 * @return List of changed resources that have been reloaded
	 */
	public Collection<Resource> handleChangedResources(Collection<Resource> changedResources,
			Collection<IEditorPart> editorsNotToRefresh, Collection<IEditorPart> editorsToRefresh) {
		Control ctrl = getControl();
		if (ctrl == null || ctrl.isDisposed())
			return Collections.emptyList();

		IWorkbenchPage workbenchPage = view.getSite().getPage();
		IEditorReference[] editorReferences = workbenchPage
				.getEditorReferences();
		ArrayList<IEditorPart> dirtyEditorsWithConflict = new ArrayList<IEditorPart>();
		ArrayList<Resource> changedResourceList = new ArrayList<Resource>(changedResources);
		// find all editor with dirty conflict
		//
		for (int i = 0; i < editorReferences.length; i++) {
			IEditorReference reference = editorReferences[i];
			IEditorPart editor = reference.getEditor(true);
			if (editor instanceof MethodElementEditor && editor.isDirty()) {
				Collection<Resource> usedResources = ((MethodElementEditor) editor)
						.getUsedResources();
				check_resource: for (int j = 0; j < changedResourceList.size(); j++) {
					Resource resource = (Resource) changedResourceList.get(j);
					if (usedResources.contains(resource)) {
						dirtyEditorsWithConflict.add(editor);
						break check_resource;
					}
				}
			}
		}
		if (!dirtyEditorsWithConflict.isEmpty()) {
			Object[] result = selectDirtyEditors(dirtyEditorsWithConflict);
			if(result != null) {
				for (int i = 0; i < result.length; i++) {
					Object editor = result[i];
					if ((editorsNotToRefresh == null || !editorsNotToRefresh
							.contains(editor))
							&& (editorsToRefresh == null || !editorsToRefresh
									.contains(editor))) {
						dirtyEditorsWithConflict.remove(editor);
					}
				}
			}
			// remove all resources used by dirty editors with conflict from the
			// collection of changed resources after updating cached modification stamp
			// so they will not be prompted to reload again until the next external change
			//
			for (int i = 0; i < dirtyEditorsWithConflict.size(); i++) {
				MethodElementEditor editor = (MethodElementEditor) dirtyEditorsWithConflict
						.get(i);				
				Collection<Resource> usedResources = editor.getUsedResources();
				usedResources.retainAll(changedResourceList);
				editor.updateResourceInfos(usedResources);
				editor.ovewriteResources(usedResources);
				changedResourceList.removeAll(usedResources);
			}
		}

		if (!changedResourceList.isEmpty()) {
			// Reload the selected changed resources.
			ILibraryManager manager = (ILibraryManager) LibraryService
					.getInstance().getCurrentLibraryManager();
			if (manager != null) {
				manager.reloadResources(changedResourceList);
			}
		}
		return changedResourceList;
	}

	private Object[] selectDirtyEditors(
			ArrayList<IEditorPart> dirtyEditorsWithConflict) {
		return selectDirtyEditors(dirtyEditorsWithConflict, view.getSite().getShell());
	}

	public static Object[] selectDirtyEditors(List<?> dirtyEditors, Shell shell) {
		String title = AuthoringUIResources._UI_FileConflict_label;
		String msg = AuthoringUIResources.selectEditorsToDiscardConflictChanges;
		return selectEditors(dirtyEditors, title, msg, shell);
	}

	private static Object[] selectEditors(List<?> editors, String title, String msg, Shell shell) {
		// prompt user to discard changes in editors that conflict with outside
		// change
		//
		IStructuredContentProvider contentProvider = new IStructuredContentProvider() {
			List<?> fContents;

			public Object[] getElements(Object inputElement) {
				if (fContents != null && fContents == inputElement)
					return fContents.toArray();
				return new Object[0];
			}

			public void dispose() {
			}

			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
				if (newInput instanceof List)
					fContents = (List<?>) newInput;
				else
					fContents = null;

			}

		};
		ILabelProvider labelProvider = new LabelProvider() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
			 */
			public String getText(Object element) {
				if (element instanceof IEditorPart) {
					return ((IEditorPart) element).getTitle();
				}
				return super.getText(element);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
			 */
			public Image getImage(Object element) {
				if (element instanceof IEditorPart) {
					return ((IEditorPart) element).getTitleImage();
				}
				return super.getImage(element);
			}
		};
		ListSelectionDialog dlg = new ListSelectionDialog(shell,
				editors, contentProvider, labelProvider, msg);
		dlg.setTitle(title);
		dlg.setBlockOnOpen(true);
		dlg.open();
		return dlg.getResult();
	}

}
