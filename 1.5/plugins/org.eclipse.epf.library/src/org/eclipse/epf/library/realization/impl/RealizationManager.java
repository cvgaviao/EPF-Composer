package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.library.edit.realization.IRealizedDescriptor;
import org.eclipse.epf.library.edit.realization.IRealizedElement;
import org.eclipse.epf.library.edit.uma.Scope;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.LibUtil;
import org.eclipse.epf.library.edit.util.ProcessPropUtil;
import org.eclipse.epf.library.edit.util.ProcessScopeUtil;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.library.util.SynFreeProcessConverter;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessPackage;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class RealizationManager implements IRealizationManager {

	private Map<MethodElement, IRealizedElement> elementMap;
	private MethodConfiguration config;
	private MethodConfiguration dynamicConfig;

	private boolean caching = false;
	private IPerspectiveListener perspectiveListener;
	private boolean localTiming = true;	
	
	public boolean isCaching() {
		return caching;
	}

	public void setCaching(boolean caching) {
		this.caching = caching;
	}

	private Map<MethodElement, IRealizedElement> getElementMap() {
		if (elementMap == null) {
			elementMap = new HashMap<MethodElement, IRealizedElement>();
		}
		return elementMap;
	}
	
	public RealizationManager(MethodConfiguration config) {
		this.config = config;
		init();
	}
	
	public void clearCacheData() {
		for (IRealizedElement element : getElementMap().values()) {
			((RealizedElement) element).dispose();
		}		
		elementMap = null;
	}
	
	public void dispose() {
		clearCacheData();
		
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			window.removePerspectiveListener(perspectiveListener);
		}
	}
	
	public MethodConfiguration getConfig() {
		if (config == null) {	//Default RealizationManager instance would not have a null config
			return dynamicConfig;
		}
		return config;
	}	

	public IRealizedElement getRealizedElement(MethodElement element) {
		IRealizedElement rElement = getElementMap().get(element);
		if (rElement == null) {
			rElement = newRealizedElement(element);
			getElementMap().put(element, rElement);
		}
		return rElement;
	}
	
	public IRealizedElement removeRealizedElement(MethodElement element) {
		return getElementMap().remove(element);
	}
	
	private void init() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			perspectiveListener = new IPerspectiveListener() {
				public void perspectiveActivated(IWorkbenchPage page,
						IPerspectiveDescriptor desc) {
					clearCacheData();
				}

				public void perspectiveChanged(IWorkbenchPage page,
						IPerspectiveDescriptor desc, String id) {
					clearCacheData();
				}
			};
			window.addPerspectiveListener(perspectiveListener);
		}
	}
	
	private IRealizedElement newRealizedElement(MethodElement element) {
		RealizedElement rElement = null;
		if (element instanceof TaskDescriptor) {
			rElement = new RealizedTaskDescriptor((TaskDescriptor) element);
			
		} else if (element instanceof RoleDescriptor) {
			rElement = new RealizedRoleDescriptor((RoleDescriptor) element);
			
		} else if (element instanceof WorkProductDescriptor) {
			rElement = new RealizedWorkProductDescriptor((WorkProductDescriptor) element);
			
		}
					
		rElement.setMgr(this);
		
		return rElement;
	}
	
	private MethodElement getLinkedElement(MethodElement element) {
		if (element instanceof RoleDescriptor) {
			return ((RoleDescriptor) element).getRole();
		}
		
		if (element instanceof WorkProductDescriptor) {
			return ((WorkProductDescriptor) element).getWorkProduct();
		}
		
		if (element instanceof TaskDescriptor) {
			return ((TaskDescriptor) element).getTask();
		}
		
		return null;
	}

	public Descriptor getDescriptor(Descriptor referencingDes, Activity parentAct, MethodElement element, EReference feature) {
		Descriptor descriptor = getDescriptor_(referencingDes, parentAct, element, feature);
		if (feature == IRealizedDescriptor.ArtifactDescriptor_ContainedArtifacts) {
			return descriptor;
		}
		
		if (feature.isMany()) {
			List listValue = (List) referencingDes.eGet(feature);
			if (listValue != null) {
				listValue.add(descriptor);
			}
		} else {
			referencingDes.eSet(feature, descriptor);
		}

		return descriptor;
	}
	
	private Descriptor getDescriptor_(Descriptor referencingDes, Activity parentAct, MethodElement element, EReference feature) {
		if (parentAct == null) {
			return null;
		}
		
		for (BreakdownElement be : parentAct.getBreakdownElements()) {
			if (getLinkedElement(be) == element) {
				return (Descriptor) be;
			}
		}
		
		Descriptor descriptor = null;
		if (element instanceof Role) {
			RoleDescriptor rd = UmaFactory.eINSTANCE.createRoleDescriptor();
			rd.setRole((Role) element);
			descriptor = rd;
			
		} else if (element instanceof Task) {
			TaskDescriptor td = UmaFactory.eINSTANCE.createTaskDescriptor();
			td.setTask((Task) element);
			descriptor = td;
			
		} else if (element instanceof WorkProduct) {
			WorkProductDescriptor wpd = UmaFactory.eINSTANCE.createWorkProductDescriptor();
			wpd.setWorkProduct((WorkProduct) element);
			descriptor = wpd;
		}
		
		if (debug) {
			System.out.println("LD> Creating descriptor: " + descriptor); //$NON-NLS-1$
		}
		
		if (descriptor == null) {
			return null;
		}
		
		DescriptorPropUtil.getDesciptorPropUtil().setCreatedByReference(descriptor, true);
			
		String presentationName = element.getPresentationName();
		descriptor.setName(element.getName());
		descriptor.setPresentationName(StrUtil.isBlank(presentationName) ? element
				.getName() : presentationName);
		String guid = UmaUtil.generateGUID();
		descriptor.setBriefDescription(element.getBriefDescription());

		addToProcess(parentAct, descriptor, feature);	
		
		return descriptor;
	}
	
	private void addToProcess(Activity parent, Descriptor referencedDes, EReference feature) {
		UmaPackage up = UmaPackage.eINSTANCE;
		parent.getBreakdownElements().add(referencedDes);
		ProcessPackage pkg = (ProcessPackage) parent.eContainer();
		pkg.getProcessElements().add(referencedDes);
		
		if (feature == up.getWorkProductDescriptor_DeliverableParts()) {
			referencedDes.setSuperActivities(null);
		}
	}
	
	public void updateProcessModel(Process proc) {	
		updateProcessModel(proc, true);
	}

	private void updateProcessModel(Process proc, boolean setCacheFlag) {	
		long time = 0;
		if (timing && localTiming) {
			time = System.currentTimeMillis();
		}
		ProcessPropUtil propUtil = ProcessPropUtil.getProcessPropUtil();
		if (! propUtil.isSynFree(proc)) {
			SynFreeProcessConverter converter = new SynFreeProcessConverter(getConfig());
			converter.convertProcess(proc, false);
		}
		updateModelImpl(proc, setCacheFlag);
		if (timing && localTiming) {
			time = System.currentTimeMillis() - time;
			System.out.println("LD> updateModel: " + time); //$NON-NLS-1$
		}
	}
	
	public void updateActivityModel(Activity act) {
		updateModelImpl(act, true);
	}

	private void updateModelImpl(Activity act, boolean setCacheFlag) {
		if (setCacheFlag) {
			clearCacheData();
			setCaching(true);
		}
		
		updateModelImpl(act);
		
		if (setCacheFlag) {
			clearCacheData();
			setCaching(false);
		}
	}
	
	private void updateModelImpl(Activity act) {
		if (config == null) {
			Process proc = ProcessUtil.getProcess(act);
			Scope scope = ProcessScopeUtil.getInstance().getScope(proc);
			if (scope != null) {
				dynamicConfig = scope;
			} else {
				dynamicConfig = LibraryService.getInstance()
						.getCurrentMethodConfiguration();
			}
		}
		
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		
		Set<Descriptor> tdReferencedSet = new HashSet<Descriptor>();
		Set<Descriptor> seenSet = new HashSet<Descriptor>();
		List<Descriptor> rdwpdList = new ArrayList<Descriptor>();
		List<BreakdownElement> beList =  act.getBreakdownElements();
		for (int i = 0; i < beList.size(); i++) {
			BreakdownElement be = beList.get(i);
			if (be instanceof Activity) {
				updateModelImpl((Activity) be);
				
			} else if (be instanceof TaskDescriptor) {
				TaskDescriptor td = (TaskDescriptor) be;
				collectAllReferences(td, tdReferencedSet, seenSet);

			} else if (be instanceof RoleDescriptor) {
				RoleDescriptor rd = (RoleDescriptor) be;
				rdwpdList.add(rd);
				
			} else if (be instanceof WorkProductDescriptor) {
				WorkProductDescriptor wpd = (WorkProductDescriptor) be;
				rdwpdList.add(wpd);
			}
		}
		
		for (Descriptor des : rdwpdList) {
			if (des instanceof TaskDescriptor || !propUtil.isCreatedByReference(des)) {
				continue;
			}
			if (!tdReferencedSet.contains(des)) {
				act.getBreakdownElements().remove(des);
			}
		}
		
		beList =  act.getBreakdownElements();
		for (int i = 0; i < beList.size(); i++) {
			BreakdownElement be = beList.get(i);
			if (be instanceof Descriptor) {
				RealizedDescriptor rdes = (RealizedDescriptor) getRealizedElement(be);	
				rdes.updatePlainTextValues();
			}
		}
				
	}
	
	private void collectAllReferences(Descriptor des, Set<Descriptor> collectingSet, Set<Descriptor> seenSet) {
		if (seenSet.contains(des)) {
			return;
		}
		seenSet.add(des);
		
		RealizedDescriptor rdes = (RealizedDescriptor) getRealizedElement(des);
		Set<Descriptor> references = rdes.getAllReferenced();
		collectingSet.addAll(references);
		for (Descriptor ref : references) {
			collectAllReferences(ref, collectingSet, seenSet);
		}
	}
	
	public void updateAllProcesseModels() {		
		boolean oldLocalTiming = localTiming;
		long time = 0;
		if (timing) {
			time = System.currentTimeMillis();
			localTiming = false;
		}

		clearCacheData();
		setCaching(true);
		for (Process proc : LibUtil.getInstance().collectProcessesFromConfig(
				getConfig())) {
			updateProcessModel(proc, false);
		}
		clearCacheData();
		setCaching(false);
		if (timing) {
			time = System.currentTimeMillis() - time;
			System.out.println("LD> beginPublish: " + time); //$NON-NLS-1$
			localTiming = oldLocalTiming;
		}
	}

	
}
