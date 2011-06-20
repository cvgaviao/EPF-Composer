package org.eclipse.epf.toolbox.libutil;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.CommonPlugin;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.DeliveryProcess;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessPackage;
import org.eclipse.epf.uma.Task;

public class LibUtil {

	private static boolean debug = ToolboxPlugin.getDefault().isDebugging();
	private static LibUtil instance = new LibUtil();
	public static LibUtil getInstance() {
		return instance;
	}
	
	public static Set<Descriptor> collectDescriptors(MethodLibrary lib) {
		Set<Descriptor> descriptors = new HashSet<Descriptor>();
		if (lib == null) {
			return descriptors;
		}
		for (Iterator iter = lib.eAllContents(); iter.hasNext();) {
			EObject element = (EObject) iter.next();
			if (element instanceof Descriptor) {
				descriptors.add((Descriptor) element);
			}
		}
		return descriptors;
	}

	public static Set<Activity> collectActivities(Process proc) {
		Set<Activity> activities = new HashSet<Activity>();
		collectActivities(proc, activities);
		return activities;
	}
	
	private static void collectActivities(Activity act, Set<Activity> activities) {
		if (act == null) {
			return;
		}
		activities.add(act);
		for (BreakdownElement be : act.getBreakdownElements()) {
			if (be instanceof Activity) {
				collectActivities((Activity) be, activities);
			}
		}
	}
	
	public static Set<Process> collectProcesses(MethodLibrary lib) {
		Set<Process> processes = new HashSet<Process>();
		if (lib == null) {
			return processes;
		}
		for (Iterator iter = lib.eAllContents(); iter.hasNext();) {
			EObject element = (EObject) iter.next();
			if (element instanceof MethodElement) {
				try {
					for (Iterator iterator = element.eCrossReferences()
							.iterator(); iterator.hasNext();) {
						Object obj = iterator.next();
						if (obj instanceof MethodElement) {
							processElement((MethodElement) obj, processes);
						}
					}
				} catch (Exception e) {
					CommonPlugin.INSTANCE.log(e);
					if (debug) {
						System.err
								.println("Error iterate thru cross references of element: " + element); //$NON-NLS-1$
					}
				}
				
				processElement((MethodElement) element, processes);
			}
		}
		return processes;
	}

	public static void processElement(MethodElement element, Set<Process> processes) {
		if (element instanceof CapabilityPattern ||
			element instanceof DeliveryProcess) {
			Process process = (Process) element;
			if (process.eContainer() instanceof ProcessComponent) {
				processes.add((Process)element);
			}
		}
	}

	public static void trace(String line) {
		ToolboxPlugin.getDefault().getLogger().logInfo(line);
		System.out.println(line);
	}
	
	public class CollectElementFilter {
		public boolean accept(MethodElement element) {
			return true;
		}
		
		public boolean skipChildren(MethodElement element) {
			if (element instanceof MethodPlugin) {
				return false;
			}
			if (element instanceof MethodPackage) {
				return false;
			}
			
			return true;
		}
		
	}
	
	private void collectElements(MethodElement element,
			CollectElementFilter filter, Set<MethodElement> collected,
			Set<MethodElement> processed) {
		if (processed.contains(element)) {
			return;
		}
		processed.add(element);

		if (filter.accept(element)) {
			collected.add(element);
		}
		if (filter.skipChildren(element)) {
			return;
		}

		EList<EReference> refList = element.eClass().getEAllContainments();
		if (refList == null || refList.isEmpty()) {
			return;
		}
		for (EReference ref : refList) {
			Object obj = element.eGet(ref);
			if (obj instanceof MethodElement) {
				collectElements((MethodElement) obj, filter, collected,
						processed);

			} else if (obj instanceof List) {
				List list = (List) obj;
				for (Object itemObj : list) {
					if (itemObj instanceof MethodElement) {
						collectElements((MethodElement) itemObj, filter,
								collected, processed);
					}
				}
			}
		}
	}
		
	public Set<? extends MethodElement> getElementsUnder(MethodElement topElement, CollectElementFilter filter) {
		Set<MethodElement> set = new HashSet<MethodElement>();
		collectElements(topElement, filter, set, new HashSet<MethodElement>());
		return set;
	}
	
	public Set<Task> getAllTasks(MethodElement topElement) {
		CollectElementFilter filter = new CollectElementFilter() {
			public boolean accept(MethodElement element) {
				return element instanceof Task;
			}
			
			public boolean skipChildren(MethodElement element) {
				if (element instanceof ProcessPackage) {
					return true;
				}
				
				return super.skipChildren(element);
			}
		};
		return (Set<Task> ) getElementsUnder(topElement, filter);
	}
	
}
