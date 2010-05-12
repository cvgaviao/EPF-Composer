package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.WorkProduct;

public class WorkProductPropUtil extends MethodElementPropUtil {

	
	public static final String WORKPRODUCT_States = "Workproduct_states";		//$NON-NLS-1$
	
	private static WorkProductPropUtil workProductPropUtil = new WorkProductPropUtil();
	public static WorkProductPropUtil getWorkProductPropUtil() {
		return workProductPropUtil;
	}
	
	public static WorkProductPropUtil getWorkProductPropUtil(IActionManager actionManager) {
		return new WorkProductPropUtil(actionManager);
	}
		
	protected WorkProductPropUtil() {		
	}
	
	protected WorkProductPropUtil(IActionManager actionManager) {
		super(actionManager);
	}
	
	public Set<Constraint> getAllStates(WorkProduct wp) {
		Set<WorkProduct> wpSet = (Set<WorkProduct>) LibraryEditUtil.getInstance()
				.collectVariabilityRelatives(wp);

		Set<Constraint> stateSet = new HashSet<Constraint>();

		for (WorkProduct w : wpSet) {
			List<Constraint> stateList = getWorkProductStates(wp);
			stateSet.addAll(stateList);
		}

		return stateSet;
	}
	
	public Constraint getState(WorkProduct wp, String stateName, boolean create) {		
		return ConstraintManager.getWorkProductState(wp, stateName, create, getActionManager());
	}
	
	public String getStateName(Constraint state) {
		if (state != null && state.getName().equals(ConstraintManager.Plugin_wpState)) {
			return state.getBody();
		}
		
		return null;
	}
	
	public List<Constraint> getWorkProductStates(WorkProduct wp) {
		List<Constraint> list = new ArrayList<Constraint>();
		
		String propValue = getStringValue(wp, WORKPRODUCT_States);
		if (propValue == null) {
			return list;
		}
		String[] guidList = propValue.split(infoSeperator); 
		if (guidList == null || guidList.length == 0) {
			return list;
		}
		for (String guid : guidList) {
			MethodElement element = LibraryEditUtil.getInstance().getMethodElement(guid);
			if (element instanceof Constraint) {
				Constraint c = (Constraint) element;
				if (c.getName().equals(ConstraintManager.Plugin_wpState)) {
					list.add(c);
				}
			}
		}
				
		return list;
	}
	
	public void addWorkProductState(WorkProduct wp, String stateName) {
		String oldValue = getStringValue(wp, WORKPRODUCT_States);
		
		//No old state
		if (oldValue == null || oldValue.trim().length() == 0) {
			Constraint state = getState(wp, stateName, true);
			setStringValue(wp, WORKPRODUCT_States, state.getGuid());
			return;
		}
		
		//Find if a state with the same name exists
		String[] guidList = oldValue.split(infoSeperator);
		for (String guid : guidList) {
			MethodElement element = LibraryEditUtil.getInstance()
					.getMethodElement(guid);
			if (element instanceof Constraint) {
				Constraint c = (Constraint) element;
				if (c.getName().equals(ConstraintManager.Plugin_wpState)
						&& c.getBody().equals(stateName)) {
					return;
				}
			}
		}
		
		//Append the new state
		Constraint state = getState(wp, stateName, true);
		String newValue = oldValue.concat(infoSeperator).concat(state.getGuid());
		setStringValue(wp, WORKPRODUCT_States, newValue);
		
	}
	
	public void removeWorkProductState(WorkProduct wp, String stateName) {
		String oldValue = getStringValue(wp, WORKPRODUCT_States);

		if (oldValue == null || oldValue.trim().length() == 0) {
			return;
		}

		boolean modified = false;
		String newValue = ""; 			//$NON-NLS-1$
		String[] guidList = oldValue.split(infoSeperator);
		for (String guid : guidList) {
			MethodElement element = LibraryEditUtil.getInstance()
					.getMethodElement(guid);
			if (element instanceof Constraint) {
				Constraint c = (Constraint) element;
				if (c.getName().equals(ConstraintManager.Plugin_wpState)) {
					if (c.getBody().equals(stateName)) {
						modified = true;
					} else {
						if (newValue.length() > 0) {
							newValue = newValue.concat(infoSeperator);
						}
						newValue = newValue.concat(c.getGuid());
					}
				}
			}
		}
		
		if (modified) {
			setStringValue(wp, WORKPRODUCT_States, newValue);
		}
		
	}
	
	
}
