package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.library.edit.realization.IRealizedElement;
import org.eclipse.epf.library.edit.realization.RealizationContext;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.epf.uma.util.UmaUtil;

public class RealizationManager implements IRealizationManager {

	private Map<MethodElement, IRealizedElement> elementMap;
	private RealizationContext context;
	private Map<Activity, List<Descriptor>> actDescrptorsMap;
	
	public RealizationManager(RealizationContext context) {
		this.context = context;
		init();
	}
	
	public void dispose() {
		elementMap = null;
		context = null;
		actDescrptorsMap = null;
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
	
	public Descriptor getDescriptor(Activity parentAct, MethodElement element) {
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
	

	
}
