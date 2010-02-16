package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.realization.IRealizedTaskDescriptor;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;

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
		EReference tdReference = up.getTaskDescriptor_PerformedPrimarilyBy();
		List<RoleDescriptor> rdList = (List<RoleDescriptor>) getCachedValue(tdReference);
		if (rdList == null) {
			EReference[] tdFeatures = {
					tdReference,
					up.getTaskDescriptor_PerformedPrimarilyByExcluded(),
					up.getTaskDescriptor_PerformedPrimarilyByIncluded(),
			};
			rdList = getRdList(up.getTask_PerformedBy(), tdFeatures);
			storeCachedValue(tdReference, rdList);
		}
		return rdList;
	}

	public List<RoleDescriptor> getAdditionallyPerformedBy() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_AdditionallyPerformedBy();
		List<RoleDescriptor> rdList = (List<RoleDescriptor>) getCachedValue(tdReference);
		if (rdList == null) {
			EReference[] tdFeatures = {
					tdReference,
					up.getTaskDescriptor_AdditionallyPerformedByExclude(),
					up.getTaskDescriptor_AdditionallyPerformedByInclude(),
			};
			rdList = getRdList(up.getTask_AdditionallyPerformedBy(), tdFeatures);
			storeCachedValue(tdReference, rdList);
		}
		return rdList;
	}
	
	private List<RoleDescriptor> getRdList(EReference tFeature,
			EReference[] tdFeatures) {
		ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(getConfig());
		
		EReference tdFeature = tdFeatures[0];

		Task task = (Task) getLinkedElement();
		if (task == null) {
			return ConfigurationHelper.calc0nFeatureValue(getTaskDescriptor(),
					tdFeature, realizer);
		}
		
		EReference tdFeatureExclude = tdFeatures[1];
		EReference tdFeatureInclude = tdFeatures[2];

		List<Role> roleList = ConfigurationHelper.calc0nFeatureValue(task,
				tFeature, realizer);
		List<RoleDescriptor> resultRdList = new ArrayList<RoleDescriptor>();
		if (roleList == null || roleList.isEmpty()) {
			return resultRdList;
		}
		Set<Role> roleSet = new LinkedHashSet<Role>(roleList);

		List<RoleDescriptor> rdList = ConfigurationHelper.calc0nFeatureValue(
				getTaskDescriptor(), tdFeature, realizer);

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
			RoleDescriptor rd = (RoleDescriptor) getMgr().getDescriptor(
					getTaskDescriptor(), parentAct, role, tdFeature);
			resultRdList.add(rd);
		}

		return resultRdList;
	}
	
	public List<WorkProductDescriptor> getMandatoryInput() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_MandatoryInput();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(tdReference);
		if (wpdList == null) {
			EReference[] tdFeatures = {
					tdReference,
					up.getTaskDescriptor_MandatoryInputExclude(),
					up.getTaskDescriptor_MandatoryInputInclude(),
			};
			wpdList = getWpdList(up.getTask_MandatoryInput(), tdFeatures);
			storeCachedValue(tdReference, wpdList);
		}
		return wpdList;
	}
	
	public List<WorkProductDescriptor> getOptionalInput() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_OptionalInput();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(tdReference);
		if (wpdList == null) {
			EReference[] tdFeatures = {
					tdReference,
					up.getTaskDescriptor_OptionalInputExclude(),
					up.getTaskDescriptor_OptionalInputInclude(),
			};
			wpdList = getWpdList(up.getTask_OptionalInput(), tdFeatures);
			storeCachedValue(tdReference, wpdList);
		}
		return wpdList;
	}
	
	public List<WorkProductDescriptor> getOutput() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_Output();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(tdReference);
		if (wpdList == null) {
			EReference[] tdFeatures = {
					tdReference,
					up.getTaskDescriptor_OutputExclude(),
					up.getTaskDescriptor_OutputInclude(),
			};
			wpdList = getWpdList(up.getTask_Output(), tdFeatures);
			storeCachedValue(tdReference, wpdList);
		}
		return wpdList;
	}
		
	protected MethodElement getLinkedElement() {
		return getTaskDescriptor().getTask();
	}
	
	private TaskDescriptor getTaskDescriptor() {
		return (TaskDescriptor) getElement();
	}
	
}
