package org.eclipse.epf.library.realization.impl;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.realization.IRealizedRoleDescriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.UmaPackage;

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
	
	protected MethodElement getLinkedElement() {
		return getRoleDescriptor().getRole();
	}
	
	private RoleDescriptor getRoleDescriptor() {
		return (RoleDescriptor) getElement();
	}
	
}
