package org.eclipse.epf.toolbox.actions;

import java.util.Set;

import org.eclipse.epf.authoring.ui.editors.EditorChooser;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.libutil.LibUtil;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public class ConvertToConfigFree implements IWorkbenchWindowActionDelegate {

	private static boolean debug = true;			//ToolboxPlugin.getDefault().isDebugging();
	
	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub

	}

	public void run(IAction action) {
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
				proc.setDefaultContext(null);
				proc.getValidContext().clear();
				n++;
			}
		}
		if (debug) {
			LibUtil.trace("LD> number of processes converted: " + n);
			LibUtil.trace("LD> number of processes not converted: " + (processes.size() - n));
		}
		
		if ( n > 0) {
			try {
				LibraryUtil.saveAll(lib);
			} catch (Exception e) {
				ToolboxPlugin.getDefault().getLogger().logError(e);
			}
		}
		
		if (debug) {
			LibUtil.trace("LD> End: ConvertToConfigFree.run()");
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

}
