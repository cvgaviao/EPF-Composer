package org.eclipse.epf.toolbox.actions;

import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.common.CommonPlugin;
import org.eclipse.epf.diagram.core.DiagramCorePlugin;
import org.eclipse.epf.diagram.core.services.DiagramHelper;
import org.eclipse.epf.diagram.core.services.DiagramManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.util.IDiagramManager;
import org.eclipse.epf.services.Services;
import org.eclipse.epf.services.ILibraryPersister.FailSafeMethodLibraryPersister;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.libutil.LibUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public class DiagramDoctor implements IWorkbenchWindowActionDelegate {

	private static boolean debug = ToolboxPlugin.getDefault().isDebugging();

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub

	}

	private void log(String line) {
		ToolboxPlugin.getDefault().getLogger().logInfo(line);
	}

	public void run(IAction action) {
		if (debug) {
			System.out.println("");
			System.out.println("LD> Begin: ConvertTosynFree.run()");
		}
		log("LD> Begin: DiagramDoctor.run()");
		MethodLibrary lib = LibraryService.getInstance()
				.getCurrentMethodLibrary();

		//		Converstion test		
		//		SynFreeProcessConverter converter = new SynFreeProcessConverter();	
		//		converter.convertLibrary(lib);

		//		Diagram clean-up test
		Set<Process> processes = LibUtil.collectProcesses(lib);
		for (Process proc : processes) {
			System.out.println("LD> proc: " + proc);
			DiagramManager mgr = DiagramManager.getInstance(proc, this);

			for (Activity act : LibUtil.collectActivities(proc)) {
				cleanupDiagrams(mgr, act);
			}

			System.out.println("");
		}

		if (debug) {
			System.out.println("LD> End: ConvertTosynFree.run()");
		}
		log("LD> End: DiagramDoctor.run()");
	}

	private void cleanupDiagrams(DiagramManager mgr, Activity act) {
		try {
			List<Diagram> diagrams = mgr.getDiagrams(act,
					IDiagramManager.ACTIVITY_DIAGRAM);
			Resource resource = null;
			boolean toDelete = false;
			int count = 0;
			for (Diagram diagram : diagrams) {
				if (toDelete) {
					DiagramHelper.deleteDiagram(diagram, false);
					count++;
				} else {
					resource = diagram.eResource();
					toDelete = true;
				}
			}

			FailSafeMethodLibraryPersister persister = Services
					.getLibraryPersister(Services.XMI_PERSISTENCE_TYPE)
					.getFailSafePersister();
			try {
				persister.save(resource);
				persister.commit();
			} catch (Exception e) {
				CommonPlugin.getDefault().getLogger().logError(e);
				persister.rollback();
				throw new CoreException(new Status(IStatus.ERROR,
						DiagramCorePlugin.PLUGIN_ID, 1, e
								.getLocalizedMessage(), null));
			}
			if (count > 0) {
				if (debug) {
					System.out.println("LD> activity: " + act);
					System.out.println("LD> Removed data count: " + count);
				}
				log("LD> activity: " + act);
				log("LD> Removed data count: " + count);
			}
		} catch (Exception e) {
			e.printStackTrace();
			log(e.getMessage());
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

}
