package org.eclipse.epf.toolbox.libutil;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.emf.common.CommonPlugin;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.DeliveryProcess;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;

public class LibUtil {

	private static boolean debug = ToolboxPlugin.getDefault().isDebugging();
	
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

	
	
}
