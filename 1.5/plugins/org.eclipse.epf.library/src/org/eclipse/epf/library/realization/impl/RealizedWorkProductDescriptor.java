package org.eclipse.epf.library.realization.impl;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.realization.IRealizedWorkProductDescriptor;
import org.eclipse.epf.uma.Deliverable;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class RealizedWorkProductDescriptor extends
		RealizedDescriptor implements IRealizedWorkProductDescriptor {

	private static Set<EStructuralFeature> featureSet = new HashSet<EStructuralFeature>();
	static {
		UmaPackage up = UmaPackage.eINSTANCE;		
//		featureSet.add(up.getNamedElement_Name());
//		featureSet.add(up.getMethodElement_PresentationName());
	}
	
	public RealizedWorkProductDescriptor(WorkProductDescriptor wpd) {
		super(wpd);
	}
	
	
	public boolean handleFeature(EStructuralFeature feature) {
		if (featureSet.contains(feature)) {
			return true;
		}
		return super.handleFeature(feature);
	}
	
	protected MethodElement getLinkedElement() {
		return getWorkProductDescriptor().getWorkProduct();
	}
	
	private WorkProductDescriptor getWorkProductDescriptor() {
		return (WorkProductDescriptor) getElement();
	}
	
	public List<WorkProductDescriptor> getDeliverableParts() {
		if (! (getLinkedElement() instanceof Deliverable)) {
			return Collections.EMPTY_LIST;
		}
		UmaPackage up = UmaPackage.eINSTANCE;
		EReference wpdReference = up.getWorkProductDescriptor_DeliverableParts();
		List<WorkProductDescriptor> wpdList = (List<WorkProductDescriptor>) getCachedValue(wpdReference);
		if (wpdList == null) {
			EReference[] wpdFeatures = {
					wpdReference,
					up.getWorkProductDescriptor_DeliverablePartsExclude(),
					up.getWorkProductDescriptor_DeliverablePartsInclude(),
			};
			wpdList = getWpdList(up.getDeliverable_DeliveredWorkProducts(), wpdFeatures);
			storeCachedValue(wpdReference, wpdList);
		}
		return wpdList;
	}
}
