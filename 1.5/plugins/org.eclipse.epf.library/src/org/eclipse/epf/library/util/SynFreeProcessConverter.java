package org.eclipse.epf.library.util;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.edit.util.LibUtil;
import org.eclipse.epf.persistence.MultiFileXMIResourceImpl;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class SynFreeProcessConverter {

	private boolean debug = true;
	
	private MethodConfiguration config;
	private Set<Resource> resouresToSave;
	
	
	public SynFreeProcessConverter() {		
	}
	
	public void convertLibrary(MethodLibrary lib) {
		if (lib == null) {
			return;
		}
		List<MethodPlugin> plugins = lib.getMethodPlugins();
		
		resouresToSave = new HashSet<Resource>();
		for (int i = 0; i < plugins.size(); i++) {
			convertPlugin(plugins.get(i), false);
		}
		save();
	}
		
	public void convertPlugin(MethodPlugin plugin, boolean toSave) {
		if (plugin == null) {
			return;
		}
		if (toSave) {
			resouresToSave = new HashSet<Resource>();
		}
		Set<Process> processes = LibUtil.getInstance().collectProcesses(plugin);
		for (Process proc : processes) {
			convertProcess(proc, false);
		}		
		if (toSave) {
			save();
		}
	}
	
	public void convertProcess(Process proc, boolean toSave) {
		Set<Descriptor> descriptors = LibUtil.getInstance().collectDescriptors(proc);

		if (toSave) {
			resouresToSave = new HashSet<Resource>();
		}
		for (Descriptor des : descriptors) {
			convert(des);
		}		
		if (toSave) {
			save();
		}
	}
	
	private void convert(Descriptor des) {
		Resource res = des.eResource();
		if (res == null) {
			return;
		}
		if (des instanceof TaskDescriptor) {
			convertTd((TaskDescriptor) des);
			
		} else if (des instanceof RoleDescriptor) {
			convertRd((RoleDescriptor) des);
			
		} else if (des instanceof WorkProductDescriptor) {
			convertWpd((WorkProductDescriptor) des);
			
		} else {
			return;
		}
		
		resouresToSave.add(res);
				
	}
	
	private void convertTd(TaskDescriptor td) {
		
	}
	
	private void convertRd(RoleDescriptor rd) {
		
	}
	
	private void convertWpd(WorkProductDescriptor wpd) {
		
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
	
}
