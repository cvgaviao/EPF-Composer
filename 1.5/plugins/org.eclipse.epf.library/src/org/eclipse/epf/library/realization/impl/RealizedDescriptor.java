package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.realization.IRealizedDescriptor;
import org.eclipse.epf.library.edit.realization.IRealizedElement;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public class RealizedDescriptor extends RealizedElement implements
		IRealizedDescriptor, IRealizedElement {

	private static Set<EStructuralFeature> featureSet = new HashSet<EStructuralFeature>();
	static {
		UmaPackage up = UmaPackage.eINSTANCE;		
		featureSet.add(up.getNamedElement_Name());
		featureSet.add(up.getMethodElement_PresentationName());
	}
	
	public RealizedDescriptor(Descriptor descriptor) {
		super(descriptor);
	}
	
	public boolean handleFeature(EStructuralFeature feature) {
		return featureSet.contains(feature);
	}
	
	public Object getFeatureValue(EStructuralFeature feature) {
		if (! featureSet.contains(feature)) {
			return null; 
		}		
		MethodElement linkedElement = getLinkedElement();
		if (feature instanceof EAttribute) {
			MethodElement elementUsed = linkedElement == null ? getDescriptor() : linkedElement;
			Object value = ConfigurationHelper.calcAttributeFeatureValue(elementUsed, feature, getConfig());
			return value;
		}

		return super.getFeatureValue(feature);
	}	
	
	public Object getOFeatureValue(OppositeFeature ofeature) {
		return super.getOFeatureValue(ofeature);
	}
	
	protected MethodElement getLinkedElement() {
		throw new UnsupportedOperationException();
	}
				
	protected Descriptor getDescriptor() {
		return (Descriptor) getElement();
	}
		
	protected List<? extends Descriptor> getDescriptorList(EReference elementFeature,
			EReference[] descriptorFeatures) {
		ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(getConfig());

		EReference descriptorFeature = descriptorFeatures[0];
		
		MethodElement element = getLinkedElement();
		if (element == null) {
			return ConfigurationHelper.calc0nFeatureValue(getDescriptor(),
					descriptorFeature, realizer);
		}
		
		

		List<MethodElement> elementList = ConfigurationHelper.calc0nFeatureValue(element,
				elementFeature, realizer);
		List<Descriptor> resultDescriptorList = new ArrayList<Descriptor>();
		if (elementList == null || elementList.isEmpty()) {
			return resultDescriptorList;
		}
		Set<MethodElement> elementSet = new LinkedHashSet<MethodElement>(elementList);

		List<Descriptor> descriptorList = ConfigurationHelper.calc0nFeatureValue(
				getDescriptor(), descriptorFeature, realizer);

		for (Descriptor des : descriptorList) {
			MethodElement me = getLinkedElement(des);
			if (elementSet.contains(me)) {
				resultDescriptorList.add(des);
				elementSet.remove(me);
			}
		}

		if (elementSet.isEmpty()) {
			return resultDescriptorList;
		}

		Activity parentAct = getDescriptor().getSuperActivities();
		if (parentAct == null) {
			return resultDescriptorList;
		}

		for (MethodElement me : elementSet) {
			Descriptor des = (Descriptor) getMgr().getDescriptor(
					getDescriptor(), parentAct, me, descriptorFeature);
			resultDescriptorList.add(des);
		}

		return resultDescriptorList;
	}
	
	private MethodElement getLinkedElement(Descriptor des) {
		if (des instanceof TaskDescriptor) {
			return ((TaskDescriptor) des).getTask();
		}
		if (des instanceof RoleDescriptor) {
			return ((RoleDescriptor) des).getRole();
		}
		if (des instanceof WorkProductDescriptor) {
			return ((WorkProductDescriptor) des).getWorkProduct();
		}
		
		return null;
	}
	
	
}
