package org.eclipse.epf.toolbox.actions;

import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.util.SynFreeProcessConverter;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public class ConvertTosynFree implements IWorkbenchWindowActionDelegate {

	private static boolean debug = ToolboxPlugin.getDefault().isDebugging();
	
	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	public void init(IWorkbenchWindow window) {
		// TODO Auto-generated method stub

	}

	@Override
	public void run(IAction action) {
		if (debug) {
			System.out.println("");
			System.out.println("LD> Begin: ConvertTosynFree.run()");
		}
		MethodLibrary lib = LibraryService.getInstance().getCurrentMethodLibrary();		
		SynFreeProcessConverter converter = new SynFreeProcessConverter();	
		converter.convertLibrary(lib);
		
		if (debug) {
			System.out.println("LD> End: ConvertTosynFree.run()");
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

}
