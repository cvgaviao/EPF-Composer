package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.realization.IRealizedTaskDescriptor;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;

public class RealizedTaskDescriptor extends RealizedDescriptor implements
		IRealizedTaskDescriptor {

	private static Set<EStructuralFeature> featureSet = new HashSet<EStructuralFeature>();
	static {
		UmaPackage up = UmaPackage.eINSTANCE;		
		featureSet.add(up.getTaskDescriptor_PerformedPrimarilyBy());
	}
	
	public RealizedTaskDescriptor(TaskDescriptor td) {
		super(td);
	}
	
	public boolean handleFeature(EStructuralFeature feature) {
		if (featureSet.contains(feature)) {
			return true;
		}
		return super.handleFeature(feature);
	}
	
	public Object getFeatureValue(EStructuralFeature feature) {
		if (! featureSet.contains(feature)) {
			return null; 
		}		
		UmaPackage up = UmaPackage.eINSTANCE;		
		if (feature == up.getTaskDescriptor_PerformedPrimarilyBy()) {
			return getPerformedPrimarilyBy();
		}

		return null;
	}	
	
	public List<RoleDescriptor> getPerformedPrimarilyBy() {
		UmaPackage up = UmaPackage.eINSTANCE;	
		ElementRealizer realizer = DefaultElementRealizer.newElementRealizer(getConfig());
		
		Task task = (Task) getLinkedElement();
		if (task == null) {
			return ConfigurationHelper.calc0nFeatureValue(
					getTaskDescriptor(), up
							.getTaskDescriptor_PerformedPrimarilyBy(), realizer);
		}
		
		List<Role> roleList = ConfigurationHelper.calc0nFeatureValue(task, up.getTask_PerformedBy(), realizer);				
		List<RoleDescriptor> resultRdList= new ArrayList<RoleDescriptor>();
		if (roleList == null || roleList.isEmpty()) {
			return resultRdList;
		} 
		Set<Role> roleSet = new LinkedHashSet<Role>(roleList);
		
		List<RoleDescriptor> rdList = ConfigurationHelper.calc0nFeatureValue(
				getTaskDescriptor(), up
						.getTaskDescriptor_PerformedPrimarilyBy(), realizer);

		for (RoleDescriptor rd : rdList) {
			Role role = rd.getRole();
			if (roleSet.contains(role)) {
				resultRdList.add(rd);
				roleSet.remove(role);
			}
		}
		
		if (roleSet.isEmpty()) {
			return resultRdList;
		}
		
		Activity parentAct = getTaskDescriptor().getSuperActivities();
		if (parentAct == null) {
			return resultRdList;
		}
		
		for (Role role : roleSet) {
			RoleDescriptor rd = (RoleDescriptor) getMgr().getDescriptor(parentAct, role);
			resultRdList.add(rd);
		}
		
		return resultRdList;
	}
	
	protected MethodElement getLinkedElement() {
		return getTaskDescriptor().getTask();
	}
	
	private TaskDescriptor getTaskDescriptor() {
		return (TaskDescriptor) getElement();
	}
	
}
