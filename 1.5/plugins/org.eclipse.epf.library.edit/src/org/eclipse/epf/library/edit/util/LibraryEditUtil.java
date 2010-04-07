package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
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
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class LibraryEditUtil {
	
	private static boolean debug = false;
	private static LibraryEditUtil instance = new LibraryEditUtil();
	private IRealizationManager defaultRealizationManager;
	private ILibraryEditUtilProvider provider;
	private boolean junitTest = false;

	public static LibraryEditUtil getInstance() {
		return instance;
	}
	
	private LibraryEditUtil() {		
	}
	
	public ILibraryEditUtilProvider getProvider() {
		if (provider == null) {
			provider = ExtensionManager.getLibraryEditUtilProvider();
		}
		return provider;
	}
	
	public IRealizationManager getDefaultRealizationManager() {
		return ProcessUtil.isSynFree() ? defaultRealizationManager : null;
	}

	public void setDefaultRealizationManager(
			IRealizationManager defaultRealizationManager) {
		this.defaultRealizationManager = defaultRealizationManager;
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

	public boolean isSynFree() {
		ILibraryEditUtilProvider p = getProvider();				
		return p == null ? true : p.isSynFree();
	}
	
	public MethodElement getMethodElement(String guid) {
		ILibraryEditUtilProvider p = getProvider();				
		return p == null ? null : p.getMethodElement(guid);
	}
		
	public EReference getExcludeFeature(EReference ref) {
		UmaPackage up = UmaPackage.eINSTANCE;
		if (ref == up.getTaskDescriptor_PerformedPrimarilyBy()) {
			return up.getTaskDescriptor_PerformedPrimarilyByExcluded();
		}
		
		if (ref == up.getTaskDescriptor_AdditionallyPerformedBy()) {
			return up.getTaskDescriptor_AdditionallyPerformedByExclude();				
		}
		
		if (ref == up.getTaskDescriptor_MandatoryInput()) {
			return up.getTaskDescriptor_MandatoryInputExclude();
		}
		
		if (ref == up.getTaskDescriptor_OptionalInput()) {
			return up.getTaskDescriptor_OptionalInputExclude();
		}
		
		if (ref == up.getTaskDescriptor_Output()) {
			return up.getTaskDescriptor_OutputExclude();
		}
		
		if (ref == up.getRoleDescriptor_ResponsibleFor()) {
			return up.getRoleDescriptor_ResponsibleForExclude();
		}
		
		if (ref == up.getWorkProductDescriptor_DeliverableParts()) {
			return up.getWorkProductDescriptor_DeliverablePartsExclude();
		}
		
		//...		
		return null;
	}
	
	public boolean isJunitTest() {
		return junitTest;
	}

	public void setJunitTest(boolean junitTest) {
		this.junitTest = junitTest;
	}
	
	public List<EReference> getExcludeRefList(Descriptor des) {
		List<EReference> list = new ArrayList<EReference>();
		UmaPackage up = UmaPackage.eINSTANCE;

		if (des instanceof TaskDescriptor) {
			list.add(up.getTaskDescriptor_PerformedPrimarilyByExcluded());
			list.add(up.getTaskDescriptor_AdditionallyPerformedByExclude());
			list.add(up.getTaskDescriptor_MandatoryInputExclude());
			list.add(up.getTaskDescriptor_OptionalInputExclude());
			list.add(up.getTaskDescriptor_OutputExclude());

		} else if (des instanceof RoleDescriptor) {
			list.add(up.getRoleDescriptor_ResponsibleForExclude());

		} else if (des instanceof WorkProductDescriptor) {
			list.add(up.getWorkProductDescriptor_DeliverablePartsExclude());
		}

		return list;
	}
	
}