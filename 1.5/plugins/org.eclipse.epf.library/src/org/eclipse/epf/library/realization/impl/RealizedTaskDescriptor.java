package org.eclipse.epf.library.realization.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.realization.IRealizedTaskDescriptor;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
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
			return super.getFeatureValue(feature); 
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
			EReference[] tdFeatures = { tdReference,
					up.getTaskDescriptor_PerformedPrimarilyByExcluded(), };
			rdList = (List<RoleDescriptor>) getDescriptorList(up
					.getTask_PerformedBy(), tdFeatures);
			storeCachedValue(tdReference, rdList);
		}
		return rdList;
	}

	public List<RoleDescriptor> getAdditionallyPerformedBy() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_AdditionallyPerformedBy();
		List<RoleDescriptor> rdList = (List<RoleDescriptor>) getCachedValue(tdReference);
		if (rdList == null) {
			EReference[] tdFeatures = { tdReference,
					up.getTaskDescriptor_AdditionallyPerformedByExclude(),};
			rdList = (List<RoleDescriptor>) getDescriptorList(up
					.getTask_AdditionallyPerformedBy(), tdFeatures);
			storeCachedValue(tdReference, rdList);
		}
		return rdList;
	}
	
	public List<WorkProductDescriptor> getMandatoryInput() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_MandatoryInput();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(tdReference);
		if (wpdList == null) {
			EReference[] tdFeatures = { tdReference,
					up.getTaskDescriptor_MandatoryInputExclude(),};
			wpdList = (List<WorkProductDescriptor>) getDescriptorList(up
					.getTask_MandatoryInput(), tdFeatures);
			storeCachedValue(tdReference, wpdList);
		}
		return wpdList;
	}
	
	public List<WorkProductDescriptor> getOptionalInput() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_OptionalInput();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(tdReference);
		if (wpdList == null) {
			EReference[] tdFeatures = { tdReference,
					up.getTaskDescriptor_OptionalInputExclude(), };
			wpdList = (List<WorkProductDescriptor>) getDescriptorList(up
					.getTask_OptionalInput(), tdFeatures);
			storeCachedValue(tdReference, wpdList);
		}
		return wpdList;
	}
	
	public List<WorkProductDescriptor> getOutput() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference tdReference = up.getTaskDescriptor_Output();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(tdReference);
		if (wpdList == null) {
			EReference[] tdFeatures = { tdReference,
					up.getTaskDescriptor_OutputExclude(), };
			wpdList = (List<WorkProductDescriptor>) getDescriptorList(up
					.getTask_Output(), tdFeatures);
			storeCachedValue(tdReference, wpdList);
		}
		return wpdList;
	}
		
	protected MethodElement getRawLinkedElement() {
		return getTaskDescriptor().getTask();
	}
	
	private TaskDescriptor getTaskDescriptor() {
		return (TaskDescriptor) getElement();
	}
	
	@Override
	public Set<Descriptor> getAllReferenced() {
		Set<Descriptor> referenced = new HashSet<Descriptor>();
		addToSet(referenced, getPerformedPrimarilyBy());
		addToSet(referenced, getMandatoryInput());
		addToSet(referenced, getAdditionallyPerformedBy());
		addToSet(referenced, getOptionalInput());
		addToSet(referenced, getOutput());
		return referenced;
	}
	
}
