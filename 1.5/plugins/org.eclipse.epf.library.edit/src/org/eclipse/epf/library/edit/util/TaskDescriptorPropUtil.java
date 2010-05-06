package org.eclipse.epf.library.edit.util;

import java.util.List;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class TaskDescriptorPropUtil extends DescriptorPropUtil {

	private static TaskDescriptorPropUtil taskDescriptorPropUtil = new TaskDescriptorPropUtil();
	public static TaskDescriptorPropUtil getTaskDescriptorPropUtil() {
		return taskDescriptorPropUtil;
	}
	
	public static TaskDescriptorPropUtil getTaskDescriptorPropUtil(IActionManager actionManager) {
		return new TaskDescriptorPropUtil(actionManager);
	}
	
	protected TaskDescriptorPropUtil() {		
	}
	
	protected TaskDescriptorPropUtil(IActionManager actionManager) {
		super(actionManager);
	}
	
	public void addWpState(TaskDescriptor td, WorkProductDescriptor wpd,
			Constraint state, EReference ref) {
		ConstraintManager.addWpState(td, wpd, state, ref, getActionManager());
	}

	public void removeWpState(TaskDescriptor td,
			WorkProductDescriptor wpd, Constraint state, EReference ref) {
		ConstraintManager.removeWpState(td, wpd, state, ref, getActionManager());
	}

	public List<Constraint> getWpStates(TaskDescriptor td,
			WorkProductDescriptor wpd, EReference ref) {
		return ConstraintManager.getWpStates(td, wpd, ref);
	}
	
	public List<Constraint> getWpInputStates(TaskDescriptor td,
			WorkProductDescriptor wpd, EReference ref) {
		UmaPackage up = UmaPackage.eINSTANCE;
		List<Constraint> list;		
		list = ConstraintManager.getWpStates(td, wpd, up
				.getTaskDescriptor_MandatoryInput());
		if (! list.isEmpty()) {
			return list;
		}
		list = ConstraintManager.getWpStates(td, wpd, up
				.getTaskDescriptor_OptionalInput());
		if (! list.isEmpty()) {
			return list;
		}
		list = ConstraintManager.getWpStates(td, wpd, up
				.getTaskDescriptor_ExternalInput());
		if (! list.isEmpty()) {
			return list;
		}		

		return list;
	}
	
	public List<Constraint> getWpOutputStates(TaskDescriptor td,
			WorkProductDescriptor wpd, EReference ref) {
		UmaPackage up = UmaPackage.eINSTANCE;

		List<Constraint> list;
		list = ConstraintManager.getWpStates(td, wpd, up
				.getTaskDescriptor_Output());
		if (!list.isEmpty()) {
			return list;
		}

		return list;
	}
	
}
