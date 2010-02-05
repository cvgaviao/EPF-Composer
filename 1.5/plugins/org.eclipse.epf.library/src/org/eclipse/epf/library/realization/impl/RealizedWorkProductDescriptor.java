package org.eclipse.epf.library.realization.impl;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.realization.IRealizedWorkProductDescriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.TaskDescriptor;
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
	
	
}
