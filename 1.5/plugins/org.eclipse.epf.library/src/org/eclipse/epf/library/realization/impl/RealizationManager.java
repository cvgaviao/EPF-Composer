package org.eclipse.epf.library.realization.impl;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.realization.IRealizationManager;
import org.eclipse.epf.library.realization.IRealizedElement;
import org.eclipse.epf.library.realization.RealizationContext;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class RealizationManager implements IRealizationManager {

	private Map<MethodElement, IRealizedElement> elementMap;
	private RealizationContext context;
	
	public RealizationManager(RealizationContext context) {
		this.context = context;
		init();
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
	
	
}
