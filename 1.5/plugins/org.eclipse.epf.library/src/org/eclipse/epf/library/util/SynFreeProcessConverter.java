package org.eclipse.epf.library.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.edit.util.MethodLibraryPropUtil;
import org.eclipse.epf.library.edit.util.MethodPluginPropUtil;
import org.eclipse.epf.library.edit.util.ProcessPropUtil;
import org.eclipse.epf.persistence.MultiFileXMIResourceImpl;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.Deliverable;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class SynFreeProcessConverter {

	private boolean debug = true;
	
	private ElementRealizer realizer;
	
	private MethodConfiguration config;
	
	private ElementRealizer getRealizer() {
		return realizer;
	}

	private void setRealizer(ElementRealizer realizer) {
		this.realizer = realizer;
	}

	private Set<Resource> resouresToSave;
	
	private UmaPackage up = UmaPackage.eINSTANCE;
	
	public SynFreeProcessConverter() {		
	}
	
	public SynFreeProcessConverter(MethodConfiguration config) {
		this.config = config;
	}
	
	private MethodConfiguration getConfig() {
		return config;
	}
	
	public void convertLibrary(MethodLibrary lib) {
		convertLibrary(lib, true);
	}
	
	public void convertLibrary(MethodLibrary lib,  boolean toSave) {
		if (debug) {
			System.out.println("LD> Begin convertLibrary: " + lib);
			System.out.println("");
		}
		convertLibrary_(lib, toSave);
		if (debug) {
			System.out.println("LD> End convertLibrary: " + lib);
			System.out.println("");
		}
	}
	
	private void convertLibrary_(MethodLibrary lib,  boolean toSave) {
		if (lib == null) {
			return;
		}
		List<MethodPlugin> plugins = lib.getMethodPlugins();
		
		if (toSave) {
			resouresToSave = new HashSet<Resource>();
		}
		for (int i = 0; i < plugins.size(); i++) {
			convertPlugin(plugins.get(i), false);
		}
		
		MethodLibraryPropUtil propUtil = MethodLibraryPropUtil.getMethodLibraryPropUtil();
		propUtil.setSynFree(lib, true);
		if (toSave) {
			resouresToSave.add(lib.eResource());
			save();
		}
	}
		
	public void convertPlugin(MethodPlugin plugin, boolean toSave) {
		if (plugin == null) {
			return;
		}
		if (debug) {
			System.out.println("LD> convertPlugin: " + plugin);
			System.out.println("");
		}
		if (toSave) {
			resouresToSave = new HashSet<Resource>();
		}
		Set<Process> processes = LibraryEditUtil.getInstance().collectProcesses(plugin);
		for (Process proc : processes) {
			convertProcess(proc, false);
		}		
		
		MethodPluginPropUtil propUtil = MethodPluginPropUtil.getMethodPluginPropUtil();
		propUtil.setSynFree(plugin, true);
		if (toSave) {
			save();
		}
	}
	
	public void convertProcess(Process proc, boolean toSave) {
		if (proc == null || proc.getDefaultContext() == null) {
			return;
		}
		if (debug) {
			System.out.println("LD> convertProcess: " + proc);
			System.out.println("");
		}
		
		MethodConfiguration c = getConfig();
		if (c == null) {
			c = proc.getDefaultContext();
		}
		ElementRealizer r = DefaultElementRealizer.newElementRealizer(c);		
		setRealizer(r);		
		
		Set<Descriptor> descriptors = LibraryEditUtil.getInstance().collectDescriptors(proc);

		if (toSave) {
			resouresToSave = new HashSet<Resource>();
		}
		for (Descriptor des : descriptors) {
			convert(des);
		}
		ProcessPropUtil.getProcessPropUtil().setSynFree(proc, true);
		
		if (toSave) {
			save();
		}
	}
	private void convert(Descriptor des) {
		boolean oldDeliver = des.eDeliver();
		try {
			if (oldDeliver) {
				des.eSetDeliver(false);
			}
			convert_(des);
		} finally {
			if (oldDeliver) {
				des.eSetDeliver(oldDeliver);
			}
		}
	}
	
	private void convert_(Descriptor des) {
		Resource res = des.eResource();
		if (res == null) {
			return;
		}
		if (debug) {
			System.out.println("LD> convert: " + des);
		}
		
		if (des instanceof TaskDescriptor) {
			convertTd((TaskDescriptor) des);
			
		} else if (des instanceof RoleDescriptor) {
			convertRd((RoleDescriptor) des);
			
		} else if (des instanceof WorkProductDescriptor) {
			convertWpd((WorkProductDescriptor) des);			
		
		} 
		
		convertSimpleTextAttributes(des);
		
		if (resouresToSave != null) {
			resouresToSave.add(res);	
		}
	}
	
	private void convertSimpleTextAttributes(Descriptor des) {
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		MethodElement element = propUtil.getLinkedElement(des);
		if (element == null) {
			return;
		}
		
		if (propUtil.hasNoValue(des.getName())) {
			des.setName(element.getName());		
		} else if (!des.getName().equals(element.getName())) {
			propUtil.setNameRepalce(des, true);
		}
		
		if (propUtil.hasNoValue(des.getPresentationName())) {
			des.setPresentationName(element.getPresentationName());			
		} else if (!des.getPresentationName().equals(element.getPresentationName())) {
			propUtil.setPresentationNameRepalce(des, true);
		}
		
		if (propUtil.hasNoValue(des.getBriefDescription())) {
			des.setBriefDescription(element.getBriefDescription());			
		} else if (!des.getBriefDescription().equals(element.getBriefDescription())) {
			propUtil.setBriefDesRepalce(des, true);
		}
		
	}
	
	private void convertTd(TaskDescriptor td) {
		Task task = (Task) getLinkedElement(td);
		if (task == null) {
			return;
		}

		convertManyEReference(td, task, 
					up.getTaskDescriptor_PerformedPrimarilyBy(),
					up.getTaskDescriptor_PerformedPrimarilyByExcluded(),
					up.getTask_PerformedBy());
		
		convertManyEReference(td, task, 
				up.getTaskDescriptor_AdditionallyPerformedBy(),
				up.getTaskDescriptor_AdditionallyPerformedByExclude(),
				up.getTask_AdditionallyPerformedBy());
		
		convertManyEReference(td, task, 
				up.getTaskDescriptor_MandatoryInput(),
				up.getTaskDescriptor_MandatoryInputExclude(),
				up.getTask_MandatoryInput());
		
		convertManyEReference(td, task, 
				up.getTaskDescriptor_OptionalInput(),
				up.getTaskDescriptor_OptionalInputExclude(),
				up.getTask_OptionalInput());
		
		convertManyEReference(td, task, 
				up.getTaskDescriptor_Output(),
				up.getTaskDescriptor_OutputExclude(),
				up.getTask_Output());
		
//		convertManyEReference(td, task, 
//				up.getTaskDescriptor_SelectedSteps(),
//				up.getTaskDescriptor_SelectedStepsExclude(),
//				up.getTask_Steps());
		
	}

	private void convertManyEReference(Descriptor ownerDescriptor, MethodElement ownerLinkedElement,
			EReference dFeature, EReference dFeatureExclude, EReference efeature) {
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		
		List<MethodElement> elements = ConfigurationHelper.calc0nFeatureValue(
				ownerLinkedElement, efeature, getRealizer());
		
		List<RoleDescriptor> descriptors = ConfigurationHelper.calc0nFeatureValue(
				ownerDescriptor, dFeature, getRealizer());
	
		//exclude list and local list
		List<MethodElement> excludeList = (List<MethodElement> ) ownerDescriptor.eGet(dFeatureExclude);
		List<Descriptor> localList = new ArrayList<Descriptor>();
		if (elements != null && !elements.isEmpty()) {
			Set<MethodElement> elementSet = new HashSet<MethodElement>(elements);
			
			List<MethodElement> linkedElements = new ArrayList<MethodElement>();
			Set<MethodElement> linkedElementSet = new HashSet<MethodElement>();
			
			if (descriptors != null && !descriptors.isEmpty()) {
				for (Descriptor des : descriptors) {
					MethodElement linkedElement = getLinkedElement(des);
					if (linkedElement == null) {
						localList.add(des);
					} else {
						linkedElementSet.add(linkedElement);
						if (elementSet.contains(linkedElement)) {
							propUtil.setCreatedByReference(des, true);
						} else {
							localList.add(des);
						}
					}
				}
			}
			
			for (MethodElement element : elements) {
				if (! linkedElementSet.contains(element)) {
					excludeList.add(element);
				}
			}			
		}
		
		//Handle localList
		for (Descriptor des : localList) {
			propUtil.addLocalUse(des, ownerDescriptor, dFeature);
		}
	}	
	
	
	private void convertRd(RoleDescriptor rd) {
		Role role = (Role) getLinkedElement(rd);
		if (role == null) {
			return;
		}
		
		convertManyEReference(rd, role, 
				up.getRoleDescriptor_ResponsibleFor(),
				up.getRoleDescriptor_ResponsibleForExclude(),
				up.getRole_ResponsibleFor());
	}
	
	private void convertWpd(WorkProductDescriptor wpd) {
		WorkProduct wp = (WorkProduct) getLinkedElement(wpd);
		if (wp == null) {
			return;
		}
		
		if (wp instanceof Deliverable) {
			convertManyEReference(wpd, wp, up
					.getWorkProductDescriptor_DeliverableParts(), up
					.getWorkProductDescriptor_DeliverablePartsExclude(), up
					.getDeliverable_DeliveredWorkProducts());
		}
	}

	private void save() {
		ILibraryPersister.FailSafeMethodLibraryPersister persister = LibraryServiceUtil
				.getCurrentPersister().getFailSafePersister();

		try {
			for (Iterator<Resource> it = resouresToSave.iterator(); it
					.hasNext();) {
				MultiFileXMIResourceImpl res = (MultiFileXMIResourceImpl) it
						.next();
				persister.save(res);
			}
			persister.commit();
			
		} catch (Exception e) {
			persister.rollback();
			e.printStackTrace();

		} finally {

		}
	}

	private MethodElement getLinkedElement(Descriptor des) {
		MethodElement element = DescriptorPropUtil.getDesciptorPropUtil().getLinkedElement(des);
		if (element == null) {
			return null;
		}
		return ConfigurationHelper.getCalculatedElement(element, getRealizer());
	}
	
}
