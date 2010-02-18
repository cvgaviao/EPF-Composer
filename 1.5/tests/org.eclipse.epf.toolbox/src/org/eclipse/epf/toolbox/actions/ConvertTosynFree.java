package org.eclipse.epf.toolbox.actions;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.emf.common.CommonPlugin;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.libutil.LibUtil;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
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
			System.out.println("LD> Begin: IConvertTosynFree.run()");
		}
		MethodLibrary lib = LibraryService.getInstance().getCurrentMethodLibrary();		
		Set<Descriptor> descriptors = LibUtil.collectDescriptors(lib);
		if (debug) {
			System.out.println("LD> descriptors: " + descriptors.size());
		}
		for (Descriptor des : descriptors) {
			
		}		
		
		if (debug) {
			System.out.println("LD> End: IConvertTosynFree.run()");
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

}
