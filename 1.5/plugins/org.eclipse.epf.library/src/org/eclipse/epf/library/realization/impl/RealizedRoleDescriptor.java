package org.eclipse.epf.library.realization.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.realization.IRealizedRoleDescriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class RealizedRoleDescriptor extends RealizedDescriptor implements
		IRealizedRoleDescriptor {

	private static Set<EStructuralFeature> featureSet = new HashSet<EStructuralFeature>();
	static {
		UmaPackage up = UmaPackage.eINSTANCE;		
//		featureSet.add(up.getNamedElement_Name());
//		featureSet.add(up.getMethodElement_PresentationName());
	}
	
	public RealizedRoleDescriptor(RoleDescriptor rd) {
		super(rd);
	}
		
	public boolean handleFeature(EStructuralFeature feature) {
		if (featureSet.contains(feature)) {
			return true;
		}
		return super.handleFeature(feature);
	}
	
	protected MethodElement getRawLinkedElement() {
		return getRoleDescriptor().getRole();
	}
	
	private RoleDescriptor getRoleDescriptor() {
		return (RoleDescriptor) getElement();
	}
	
	public List<WorkProductDescriptor> getResponsibleFor() {
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference rdReference = up.getRoleDescriptor_ResponsibleFor();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(rdReference);
		if (wpdList == null) {
			EReference[] rdFeatures = { rdReference,
					up.getRoleDescriptor_ResponsibleForExclude(), };
			wpdList = (List<WorkProductDescriptor>) getDescriptorList(up
					.getRole_ResponsibleFor(), rdFeatures);
			storeCachedValue(rdReference, wpdList);
		}
		return wpdList;
	}
	
}
