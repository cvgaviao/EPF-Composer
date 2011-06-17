package org.eclipse.epf.toolbox.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.PresentationContext;
import org.eclipse.epf.library.edit.util.Comparators;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.toolbox.libutil.LibUtil;
import org.eclipse.epf.uma.ContentDescription;
import org.eclipse.epf.uma.FulfillableElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Task;
import org.eclipse.jface.action.IAction;

public class DebugTest {

	public static boolean debugTestMode = true;
	public static void run(IAction action) {
		LibUtil.trace("LD> Begin: DebugTest.run()");
		
		run_(action);
		
		LibUtil.trace("LD> End:   DebugTest.run()");
	}
	
	private static void run_(IAction action) {
		debug00428551(action);
	}
	
	private static void debug00428551(IAction action) {
		MethodLibrary lib = LibraryService.getInstance().getCurrentMethodLibrary();	
		if (lib == null) {
			return;
		}
		
		MethodPlugin plugin = lib.getMethodPlugins().get(0);
		
		Set<Task>  tasks = LibUtil.getInstance().getAllTasks(plugin);
		System.out.println("LD> tasks: " + tasks.size() + "\n");
		List<Task> taskList = new ArrayList<Task>();
		taskList.addAll(tasks);
		
		Collections.sort(taskList, Comparators.DEFAULT_COMPARATOR);
		
		int cc = 0;
		for (Task task : taskList) {
			ContentDescription pres = task.getPresentation();
			if (pres.eIsProxy()) {				
				ToolboxPlugin.getDefault().getLogger().logInfo("name: " + task.getName());
				ToolboxPlugin.getDefault().getLogger().logInfo("guid: " + task.getGuid());
				ToolboxPlugin.getDefault().getLogger().logInfo("pres: " + pres.getGuid());
				ToolboxPlugin.getDefault().getLogger().logInfo("=============================");
				
				System.out.println("name: " + task.getName());
				System.out.println("guid: " + task.getGuid());
				System.out.println("pres: " + pres.getGuid());				
				System.out.println("");
				cc++;
			}
		}
		System.out.println("LD> bad tasks: " + cc);
		
	}
	
	
}
