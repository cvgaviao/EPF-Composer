package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.util.UmaUtil;

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
	
	/**
	 * Get all the states assigned to the given work-product "wp" and all its variability relatives
	 * @param wp
	 * @return a set of all states
	 */
	public Set<Constraint> getAllStates(WorkProduct wp) {
		Set<WorkProduct> wpSet = (Set<WorkProduct>) LibraryEditUtil.getInstance()
				.collectVariabilityRelatives(wp);

		Set<Constraint> stateSet = new HashSet<Constraint>();

		for (WorkProduct w : wpSet) {
			List<Constraint> stateList = getWorkProductStates(w);
			stateSet.addAll(stateList);
		}

		return stateSet;
	}
	
	/**
	 * Find the state under the plug-in containing the work-product "wp" with the given name "stateName".
	 * If such state cannot be found, return null if "create" is false, otherwise create a new state
	 * under the plug-in and return it.
	 * 
	 * Note that this API does not assign the state to the wp.
	 * 
	 * @param wp
	 * @param stateName
	 * @param create
	 * @return the state
	 */
	private Constraint getState(WorkProduct wp, String stateName, boolean create) {		
		return ConstraintManager.getWorkProductState(wp, stateName, create, getActionManager());
	}
	
	/**
	 * Get the name of the given state
	 * @param state
	 * @return the name
	 */
	public String getStateName(Constraint state) {
		if (state != null && state.getName().equals(ConstraintManager.Plugin_wpState)) {
			return state.getBody();
		}
		
		return null;
	}
	
	/**
	 * Get all the states assigned to the given work-product "wp".
	 * 
	 * @param wp
	 * @return the states
	 */
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
		
		MethodPlugin plugin = UmaUtil.getMethodPlugin(wp);
		if (plugin == null) {
			return list;
		}
		Set<Constraint> statesInPlugin = new HashSet<Constraint>();
		List<Constraint> stateListInPlugin = MethodPluginPropUtil.getMethodPluginPropUtil().getWorkProductStatesInPlugin(plugin);
		if (stateListInPlugin.isEmpty()) {
			return list;
		}
		statesInPlugin.addAll(stateListInPlugin);
		
		boolean modified = false;
		String newValue = ""; 			//$NON-NLS-1$
		for (String guid : guidList) {
			MethodElement element = LibraryEditUtil.getInstance().getMethodElement(guid);
			if (element instanceof Constraint && statesInPlugin.contains(element)) {
				Constraint c = (Constraint) element;
				if (c.getName().equals(ConstraintManager.Plugin_wpState)) {
					list.add(c);
				}
				if (newValue.length() > 0) {
					newValue = newValue.concat(infoSeperator);
				}
				newValue = newValue.concat(c.getGuid());
			} else {
				modified = true;
			}
		}
		
		
		if (modified) {
			setStringValue(wp, WORKPRODUCT_States, newValue);
		}
				
		return list;
	}
	
	/**
	 * Assign to the work-product "wp" a state with a name given by "stateName".
	 * If such a state has been assigned before, do nothing and return.
	 * If such state is already in the "wp"s plug-in, get it and assign to "wp".
	 * If such state is not in the "wp"s plug-in, create it and assign to "wp".
	 * @param wp
	 * @param stateName
	 */
	public void addWorkProductState(WorkProduct wp, Constraint srcState) {
		String stateName = srcState.getBody();
		
		String oldValue = getStringValue(wp, WORKPRODUCT_States);
		
		//No old state
		if (oldValue == null || oldValue.trim().length() == 0) {
			Constraint state = getState(wp, stateName, true);
			copyDescription(srcState, state);
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
		copyDescription(srcState, state);
		String newValue = oldValue.concat(infoSeperator).concat(state.getGuid());
		setStringValue(wp, WORKPRODUCT_States, newValue);
		
	}

	private void copyDescription(Constraint srcState, Constraint tgtState) {
		if (tgtState == srcState) {
			return;			
		}
		tgtState.setBriefDescription(srcState.getBriefDescription());
	}
	
	/**
	 * Un-assign from "wp" the state with name given by "stateName".
	 * Do nothing if such state is not assigned to "wp".
	 * @param wp
	 * @param stateName
	 */
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
