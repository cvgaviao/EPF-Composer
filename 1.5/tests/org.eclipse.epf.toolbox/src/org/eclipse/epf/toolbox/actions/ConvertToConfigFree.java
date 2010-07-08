package org.eclipse.epf.toolbox.actions;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.authoring.ui.editors.EditorChooser;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.persistence.MultiFileXMIResourceImpl;
import org.eclipse.epf.persistence.refresh.RefreshJob;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.libutil.LibUtil;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

public class ConvertToConfigFree implements IWorkbenchWindowActionDelegate {

	private static boolean debug = true;			//ToolboxPlugin.getDefault().isDebugging();
	private Set<Resource> resouresToSave = new HashSet<Resource>();
	
	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub

	}

	public void run(IAction action) {
		
		boolean oldValue = RefreshJob.getInstance().isEnabled();
		RefreshJob.getInstance().setEnabled(false);
		
		final IAction a = action;
		WorkspaceModifyOperation operation = new WorkspaceModifyOperation() {
			public void execute(IProgressMonitor monitor) {
				monitor.beginTask("Convert processes to config-free ... ",
						10);
				monitor.worked(3);
				monitor.setTaskName("Convert processes to config-free ...");
				try {
					run_(a);
				} finally {
					monitor.done();
				}
			}
		};

		try {
			// Run the operation and display the progress.
			ProgressMonitorDialog pmDialog = new ProgressMonitorDialog(Display
					.getCurrent().getActiveShell());
			pmDialog.run(true, false, operation);
		} catch (Exception e) {
			ToolboxPlugin.getDefault().getLogger().logError(e);
		} finally {
			if (oldValue) {
				RefreshJob.getInstance().setEnabled(true);
			}	
		}
	}
	
	private void run_(IAction action) {
		if (debug) {
			LibUtil.trace("LD> Begin: ConvertToConfigFree.run()");
		}
		MethodLibrary lib = LibraryService.getInstance().getCurrentMethodLibrary();	
		if (lib == null) {
			return;
		}
		EditorChooser.getInstance().closeAllMethodEditorsWithSaving();
		
		if (debug) {
			LibUtil.trace("LD> current library: " + lib);
		}
		Set<Process> processes = LibUtil.collectProcesses(lib);
		int n = 0;
		for (Process proc : processes) {
			if (ConfigurationHelper.getDelegate().canBeConfigFree(proc)) {
				if (proc.getDefaultContext() != null
						|| !proc.getValidContext().isEmpty()) {
					proc.setDefaultContext(null);
					proc.getValidContext().clear();
					n++;
					Resource res = proc.eResource();
					if (res != null) {
						resouresToSave.add(res);
					}
				}
			}
		}
		if (debug) {
			LibUtil.trace("LD> number of processes touched: " + n);
			LibUtil.trace("LD> number of processes not touched: " + (processes.size() - n));
		}
		
		if ( n > 0) {
			save();
		}
		
		if (debug) {
			LibUtil.trace("LD> End: ConvertToConfigFree.run()");
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}
	
	private void save() {
		ILibraryPersister.FailSafeMethodLibraryPersister persister = LibraryServiceUtil
				.getCurrentPersister().getFailSafePersister();

		try {
			for (Iterator<Resource> it = resouresToSave.iterator(); it
					.hasNext();) {
				MultiFileXMIResourceImpl res = (MultiFileXMIResourceImpl) it
						.next();
				persister.save(res);
			}
			persister.commit();

		} catch (Exception e) {
			persister.rollback();
			e.printStackTrace();

		} finally {

		}
	}

}
