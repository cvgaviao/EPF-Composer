package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.library.edit.realization.IRealizedElement;
import org.eclipse.epf.library.edit.realization.RealizationContext;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.LibUtil;
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
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;
import org.eclipse.epf.uma.util.AssociationHelper;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class RealizationManager implements IRealizationManager {

	private Map<MethodElement, IRealizedElement> elementMap;
	private RealizationContext context;
	private Map<Activity, List<Descriptor>> actDescrptorsMap;
	private boolean caching = false;
	private IPerspectiveListener perspectiveListener;
	private boolean localTiming = true;
	
	public boolean isCaching() {
		return caching;
	}

	public void setCaching(boolean caching) {
		this.caching = caching;
	}

	public RealizationManager(RealizationContext context) {
		this.context = context;
		if (context.getMode() == 1) {
			caching = true;
		}
		init();
	}
	
	public void clearCacheData() {
		if (IRealizationManager.debug) {
			System.out.println("LD> RealizationManger.clearCacheData: " + context); //$NON-NLS-1$
		}
		if (elementMap != null) {
			for (IRealizedElement element : elementMap.values()) {
				((RealizedElement) element).dispose();
			}
		}
		elementMap = new HashMap<MethodElement, IRealizedElement>();
		actDescrptorsMap = new HashMap<Activity, List<Descriptor>>();
	}
	
	public void dispose() {
		clearCacheData();
		elementMap = null;
		actDescrptorsMap = null;
		context = null;
		
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			window.removePerspectiveListener(perspectiveListener);
		}
	}
	
	public MethodConfiguration getConfig() {
		return context == null ? null : context.getConfig();
	}	
	
	public int getRealizationMode() {
		return context == null ? -1 : context.getMode();
	}

	public IRealizedElement getRealizedElement(MethodElement element) {
		IRealizedElement rElement = elementMap.get(element);
		if (rElement == null) {
			rElement = newRealizedElement(element);
			elementMap.put(element, rElement);
		}
		return rElement;
	}
	
	public IRealizedElement removeRealizedElement(MethodElement element) {
		return elementMap.remove(element);
	}
	
	private void init() {
		elementMap = new HashMap<MethodElement, IRealizedElement>();
		actDescrptorsMap = new HashMap<Activity, List<Descriptor>>();
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
		if (parentAct == null) {
			return null;
		}
		
		for (BreakdownElement be : parentAct.getBreakdownElements()) {
			if (getLinkedElement(be) == element) {
				return (Descriptor) be;
			}
		}
		
		List<Descriptor> dList = actDescrptorsMap.get(parentAct);
		if (dList != null) {
			for (Descriptor d : dList) {
				if (getLinkedElement(d) == element) {
					return d;
				}
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
		
		if (descriptor == null) {
			return null;
		}
		addToProcess(referencingDes, parentAct, descriptor, feature);	
		
		DescriptorPropUtil.getDesciptorPropUtil().setDynamic(descriptor, true);
		
		if (dList == null) {
			dList = new ArrayList<Descriptor>();
			actDescrptorsMap.put(parentAct, dList);
		}
		dList.add(descriptor);

			
		String presentationName = element.getPresentationName();
		descriptor.setName(element.getName());
		descriptor.setPresentationName(StrUtil.isBlank(presentationName) ? element
				.getName() : presentationName);
		String guid = UmaUtil.generateGUID();
		descriptor.setBriefDescription(element.getBriefDescription());
				
		return descriptor;
	}
	
	private void addToProcess(Descriptor ReferecingDes, Activity parent, Descriptor referencedDes, EReference feature) {
		UmaPackage up = UmaPackage.eINSTANCE;
		parent.getBreakdownElements().add(referencedDes);
		ProcessPackage pkg = (ProcessPackage) parent.eContainer();
		pkg.getProcessElements().add(referencedDes);
		if (feature.isMany()) {
			List listValue = (List) ReferecingDes.eGet(feature);
			if (listValue != null) {
				listValue.add(referencedDes);
			}
		} else {
			ReferecingDes.eSet(feature, referencedDes);
		}
		
//		if (feature == up.getTaskDescriptor_PerformedPrimarilyBy()) {
//			MultiResourceEObject mreference = (MultiResourceEObject) referencedDes;
//			mreference.oppositeAdd(AssociationHelper.RoleDescriptor_PrimaryTaskDescriptors, ReferecingDes);			
//		}
	}

	public void updateModel(Process proc) {	
		long time;
		if (timing && localTiming) {
			time = System.currentTimeMillis();
		}
		updateModelImpl(proc);
		if (timing && localTiming) {
			time = System.currentTimeMillis() - time;
			System.out.println("LD> updateModel: " + time); //$NON-NLS-1$
		}
	}
	
	private void updateModelImpl(Activity act) {
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
		
		boolean oldDeliver = act.eDeliver();
		try {
			if (oldDeliver) {
				act.eSetDeliver(false);
			}
			for (Descriptor des : rdwpdList) {
				if (des instanceof TaskDescriptor || !propUtil.isDynamic(des)) {
					continue;
				}
				if (!tdReferencedSet.contains(des)) {
					act.getBreakdownElements().remove(des);
				}
			}
		} finally {
			if (oldDeliver) {
				act.eSetDeliver(oldDeliver);
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
	
	public void beginPublish() {
		
		boolean oldLocalTiming = localTiming;
		long time;
		if (timing) {
			time = System.currentTimeMillis();
			localTiming = false;
		}

		clearCacheData();
		for (Process proc : LibUtil.getInstance().collectProcessesFromConfig(
				getConfig())) {
			updateModel(proc);
		}
		if (timing) {
			time = System.currentTimeMillis() - time;
			System.out.println("LD> beginPublish: " + time); //$NON-NLS-1$
			localTiming = oldLocalTiming;
		}
	}
	
	public void endPublish() {
		clearCacheData();
	}
	
}
