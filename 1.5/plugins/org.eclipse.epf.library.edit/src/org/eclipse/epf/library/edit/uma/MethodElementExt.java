package org.eclipse.epf.library.edit.uma;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.epf.library.edit.util.Comparators;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.ecore.IUserDefinedTypeMeta;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;
import org.eclipse.epf.uma.util.UmaUtil;

public class MethodElementExt extends ExtendObject {

	private MethodElement element;
	private boolean transientElement = false;
	private Object cachedObject;
	private ExtendReferenceMap extendReferenceMap;
	private IUserDefinedTypeMeta userDefinedTypeMeta;
	private IUserDefinedTypeMeta modifiedTypeMeta;
	private Map<Object, Object> extendedPropertyMap;

	public MethodElementExt(MethodElement element, ExtendObject oldObj) {
		if (oldObj != null) {
			copy(oldObj);
		}
		this.element = element;
	}	
	
	@Override
	protected void copy(ExtendObject oldObj) {
		super.copy(oldObj);
		if (! (oldObj instanceof MethodElementExt)) {
			return;
		}
		MethodElementExt old = (MethodElementExt) oldObj;
		element = 				old.element;
		transientElement = 		old.transientElement;
		cachedObject = 			old.cachedObject;
		extendReferenceMap =	old.extendReferenceMap;
		userDefinedTypeMeta =	old.userDefinedTypeMeta;
		modifiedTypeMeta = 		old.modifiedTypeMeta;
		extendedPropertyMap =	old.extendedPropertyMap;
	}
	
	public ExtendReferenceMap getExtendReferenceMap(boolean create) {
		if (create && extendReferenceMap == null) {
			extendReferenceMap = new ExtendReferenceMap(element);
		}
		return extendReferenceMap;
	}
	
	public Object getCachedObject() {
		return cachedObject;
	}

	public void setCachedObject(Object cachedObject) {
		this.cachedObject = cachedObject;
	}
	
	public MethodElement getElement() {
		return element;
	}
	
	public boolean isTransientElement() {
		return transientElement;
	}

	public void setTransientElement(boolean transientElement) {
		this.transientElement = transientElement;
	}
	
	public static class WorkProductStateExt extends MethodElementExt {
		private Set<WorkProduct> assignedToWps;
		
		public WorkProductStateExt(Constraint element, ExtendObject oldObj) {
			super(element, oldObj);
		}
		
		@Override
		protected void copy(ExtendObject oldObj) {
			super.copy(oldObj);
			if (! (oldObj instanceof WorkProductStateExt)) {
				return;
			}
			WorkProductStateExt old = (WorkProductStateExt) oldObj;			
			assignedToWps = old.assignedToWps;

		}
		
		public void addToAssignedToWps(WorkProduct wp) {
			if (assignedToWps == null) {
				assignedToWps = new HashSet<WorkProduct>();
			}
			assignedToWps.add(wp);
		}
		
		public void removeFromAssignedToWps(WorkProduct wp) {
			if (assignedToWps == null) {
				return;
			}
			assignedToWps.remove(wp);
		}
		
		public List<WorkProduct> getAssignedToWorkProducts() {
			List<WorkProduct> list = new ArrayList<WorkProduct>();
			List<WorkProduct> list1 = new ArrayList<WorkProduct>();
			if (assignedToWps == null) {
				return list;
			}
			for (WorkProduct wp : assignedToWps) {
				if (UmaUtil.isInLibrary(wp)) {
					list.add(wp);
				} else {
					list1.add(wp);
				}
			}
			if (! list1.isEmpty()) {
				assignedToWps.removeAll(list1);
			}
			if (list.size() > 1) {
				Collections.sort(list, Comparators.PRESENTATION_NAME_COMPARATOR);
			}
			return list;
		}
		
	}

	public IUserDefinedTypeMeta getUserDefinedTypeMeta() {
		return userDefinedTypeMeta;
	}

	public void setUserDefinedTypeMeta(IUserDefinedTypeMeta userDefinedTypeMeta) {
		this.userDefinedTypeMeta = userDefinedTypeMeta;
	}
	
	public IUserDefinedTypeMeta getModifiedTypeMeta() {
		return modifiedTypeMeta;
	}

	public void setModifiedTypeMeta(IUserDefinedTypeMeta modifiedTypeMeta) {
		this.modifiedTypeMeta = modifiedTypeMeta;
	}
	
	public Map<Object, Object> getExtendedPropertyMap(boolean create) {
		if (extendedPropertyMap == null && create) {
			extendedPropertyMap = new HashMap<Object, Object>();
		}
		return extendedPropertyMap;
	}
	
}
