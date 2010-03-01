package org.eclipse.epf.library.edit.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.DeliveryProcess;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;

public class LibUtil {
	
	private static boolean debug = false;
	private static LibUtil instance = new LibUtil();
	private Map<Process, IRealizationManager> realizationManagerMap;
	
	public static LibUtil getInstance() {
		return instance;
	}
	
	private Map<Process, IRealizationManager> getRealizationManagerMap() {
		if (realizationManagerMap == null) {
			realizationManagerMap = new HashMap<Process, IRealizationManager>();
		}
		return realizationManagerMap;
	}
	
	public void addToRealizationManagerMap(Process proc, IRealizationManager mgr) {
		getRealizationManagerMap().put(proc, mgr);
	}
	
	public void removeFromRealizationManagerMap(Process proc) {
		getRealizationManagerMap().remove(proc);
	}

	public IRealizationManager getRelizationManager(Process proc) {
		return getRealizationManagerMap().get(proc);
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

	private void collectProcess(Process process, Set<Process> processes) {
		if (process instanceof CapabilityPattern ||
				process instanceof DeliveryProcess) {
			if (process.eContainer() instanceof ProcessComponent) {
				processes.add(process);
			}
		}
	}

	public Set<Process> collectProcessesFromConfig(MethodConfiguration config) {
		Set<Process> result = new HashSet<Process>();
		List<MethodPlugin> plugins = config.getMethodPluginSelection();
		for (int i = 0; i < plugins.size(); i++) {
			Set<Process> set = collectProcesses(plugins.get(i));
			result.addAll(set);
		}
		return result;
	}

	
}
