package org.eclipse.epf.library.edit.util;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.DeliveryProcess;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;

public class LibUtil {
	
	private static boolean debug = false;
	private static LibUtil instance = new LibUtil();
	public static LibUtil getInstance() {
		return instance;
	}

	public Set<Descriptor> collectDescriptors(Process process) {			
		Set<Descriptor> descriptors = new HashSet<Descriptor>();
		
		EObject container = process.eContainer();
		if (! (container instanceof ProcessComponent)) {
			return descriptors;
		}
		
		for (Iterator iter = container.eAllContents(); iter.hasNext();) {
			EObject element = (EObject) iter.next();
			if (element instanceof Descriptor) {
				descriptors.add((Descriptor) element);
			}
		}
		return descriptors;
	}
	

	public Set<Process> collectProcesses(MethodElement libOrPlugin) {
		Set<Process> processes = new HashSet<Process>();
		if (! (libOrPlugin instanceof MethodLibrary ||
				libOrPlugin instanceof MethodPlugin)) {
			return processes;
		}
		for (Iterator iter = libOrPlugin.eAllContents(); iter.hasNext();) {
			EObject element = (EObject) iter.next();
			if (element instanceof Process) {				
				collectProcess((Process) element, processes);
			}
		}
		return processes;
	}

	public void collectProcess(Process process, Set<Process> processes) {
		if (process instanceof CapabilityPattern ||
				process instanceof DeliveryProcess) {
			if (process.eContainer() instanceof ProcessComponent) {
				processes.add(process);
			}
		}
	}


	
}
