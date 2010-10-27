package org.eclipse.epf.toolbox.actions;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.authoring.ui.editors.EditorChooser;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.edit.util.ProcessScopeUtil;
import org.eclipse.epf.persistence.MultiFileXMIResourceImpl;
import org.eclipse.epf.persistence.refresh.RefreshJob;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.libutil.LibUtil;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

public class RemoveLocalDescriptors implements IWorkbenchWindowActionDelegate {

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
				if (debug) {
					LibUtil.trace("LD> Begin: RemoveLocalDescriptors.run()");
				}

				MethodLibrary lib = LibraryService.getInstance().getCurrentMethodLibrary();	
				if (lib == null) {
					return;
				}				
				if (debug) {
					LibUtil.trace("LD> current library: " + lib);
				}
				
				EditorChooser.getInstance().closeAllMethodEditorsWithSaving();
				
				Set<Process> processes = LibUtil.collectProcesses(lib);
				int sz = processes.size();
				if (sz == 0) {
					return;
				}
				monitor.beginTask("Remove local role/workproduct descriptors ... ",
						sz);
				monitor.setTaskName("Remove local role/workproduct descriptors ... ");
				try {
					run_(a, processes, monitor);
				} finally {
					monitor.done();
				}
				
				if (debug) {
					LibUtil.trace("LD> End: RemoveLocalDescriptors.run()");
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
	
	private void run_(IAction action, Set<Process> processes,
			IProgressMonitor monitor) {
		
		int i = 0;	
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		
		Set<Process> processToSave = new HashSet<Process>();
		for (Process proc : processes) {
			i++;
			monitor.worked(i);
			if (ProcessScopeUtil.getInstance().isConfigFree(proc)) {			
				Set<Descriptor> descriptors = LibraryEditUtil.getInstance().collectDescriptors(proc);
				for (Descriptor des : descriptors) {
					if (des instanceof TaskDescriptor) {
						String value = propUtil.getStringValue(des, DescriptorPropUtil.DESCRIPTOR_LocalUsingInfo);
						if (value != null && value.trim().length() > 0) {
							propUtil.setStringValue(des, DescriptorPropUtil.DESCRIPTOR_LocalUsingInfo, "");
							processToSave.add(proc);
						}
					}
				}
			}			
		}		
		for (Process proc : processToSave) {		
			Resource res = proc.eResource();
			if (res != null) {
				resouresToSave.add(res);
			}
		}
		if (debug) {
			LibUtil.trace("LD> number of processes touched: " + processToSave.size());
			LibUtil.trace("LD> number of processes not touched: "
					+ (processes.size() - processToSave.size()));
		}

		if (processToSave.size() > 0) {
			save();
		}

	}

	public void selectionChanged(IAction action, ISelection selection) {
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
