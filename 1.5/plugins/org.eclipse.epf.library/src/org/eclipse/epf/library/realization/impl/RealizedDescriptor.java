package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.Collections;
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
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public class RealizedDescriptor extends RealizedElement implements
		IRealizedDescriptor, IRealizedElement {

	private static Set<EStructuralFeature> featureSet = new HashSet<EStructuralFeature>();
	static {
		UmaPackage up = UmaPackage.eINSTANCE;		
		featureSet.add(up.getNamedElement_Name());
		featureSet.add(up.getMethodElement_PresentationName());
		featureSet.add(up.getMethodElement_BriefDescription());
	}
	
	public RealizedDescriptor(Descriptor descriptor) {
		super(descriptor);
	}
	
	public boolean handleFeature(EStructuralFeature feature) {
		return featureSet.contains(feature);
	}
	
	public Object getFeatureValue(EStructuralFeature feature) {
		if (! featureSet.contains(feature)) {
			return super.getFeatureValue(feature); 
		}		

		if (feature instanceof EAttribute) {	

			Object value = getDescriptor().eGet(feature);
			if (getLinkedElement() == null) {
				return value;
			}
			
			DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
			if (propUtil.isValueReplaced(feature, getDescriptor())) {
				return value;
			}
			
			Object linkedValue = ConfigurationHelper.calcAttributeFeatureValue(getLinkedElement(), feature, getConfig());
			if (linkedValue != null && !linkedValue.equals(value)) {
				getDescriptor().eSet(feature, linkedValue);
			}
			
			return linkedValue;
		}

		return super.getFeatureValue(feature);
	}
	
	public Object getOFeatureValue(OppositeFeature ofeature) {
		return super.getOFeatureValue(ofeature);
	}
	
	protected MethodElement getLinkedElement() {
		MethodElement element = getRawLinkedElement();
		if (element == null) {
			return null;
		}
		return ConfigurationHelper.getCalculatedElement(element, getConfig());
	}
	
	protected MethodElement getRawLinkedElement() {
		throw new UnsupportedOperationException();
	}
				
	protected Descriptor getDescriptor() {
		return (Descriptor) getElement();
	}
		
	protected List<? extends Descriptor> getDescriptorList(EReference elementFeature,
			EReference[] descriptorFeatures) {
		ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(getConfig());

		EReference dFeature = descriptorFeatures[0];
		EReference dFeatureExclude = descriptorFeatures[1];
		
		MethodElement element = getLinkedElement();
		if (element == null) {
			return ConfigurationHelper.calc0nFeatureValue(getDescriptor(),
					dFeature, realizer);
		}
						
		List<MethodElement> elementList = ConfigurationHelper.calc0nFeatureValue(element,
				elementFeature, realizer);
		
		List<Descriptor> resultDescriptorList = new ArrayList<Descriptor>();		
		
		List<MethodElement> excludeList = 	null;		
		if (elementList != null && !elementList.isEmpty()) {
				excludeList = ConfigurationHelper.calc0nFeatureValue(getDescriptor(),
						dFeatureExclude, realizer); 
		}
		
		Set<MethodElement> excludeElements = new HashSet<MethodElement>();
		if (excludeList != null && !excludeList.isEmpty()) {
			for (MethodElement des : excludeList) {
				MethodElement elem = getLinkedElement();
				if (elem != null) {
					excludeElements.add(elem);
				}
			}
		}
		
		Set<MethodElement> elementSet = new LinkedHashSet<MethodElement>();
		if (elementList != null) {
			for (MethodElement elem : elementList) {
				if (! excludeElements.contains(elem)) {
					elementSet.add(elem);
				}
			}
		}		
		
		List<Descriptor> descriptorList = ConfigurationHelper.calc0nFeatureValue(
				getDescriptor(), dFeature, realizer);

		for (Descriptor des : descriptorList) {
			MethodElement me = getLinkedElement(des);
			if (me == null
					|| DescriptorPropUtil.getDesciptorPropUtil().localUse(des,
							getDescriptor(), dFeature)) {
				resultDescriptorList.add(des);

			} else if (elementSet.contains(me)) {
				resultDescriptorList.add(des);
				elementSet.remove(me);
			}
		}

		if (elementSet.isEmpty()) {
			return processResultDescriptorList(resultDescriptorList, dFeature);
		}

		Activity parentAct = getDescriptor().getSuperActivities();
		if (parentAct == null) {
			return processResultDescriptorList(resultDescriptorList, dFeature);
		}

		for (MethodElement me : elementSet) {
			Descriptor des = (Descriptor) getMgr().getDescriptor(
					getDescriptor(), parentAct, me, dFeature);
			resultDescriptorList.add(des);
		}

		
		return processResultDescriptorList(resultDescriptorList, dFeature);
	}
	
	private List<Descriptor> processResultDescriptorList(
			List<Descriptor> resultDescriptorList, EReference dFeature) {
		if (dFeature.isMany()) {
			List<Descriptor> listValue = (List<Descriptor>) getDescriptor().eGet(
					dFeature);
			if (listValue != null && !listValue.isEmpty()) {
				Set<Descriptor> resultSet = new HashSet<Descriptor>(
						resultDescriptorList);
				DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
				
				for (int i = listValue.size() - 1; i >= 0; i--) {
					Descriptor des = listValue.get(i);
					if (propUtil.isCreatedByReference(des) && !resultSet.contains(des)) {
						listValue.remove(i);
					}
				}
			}
		}
		
		return resultDescriptorList;
	}
	
	private MethodElement getLinkedElement(Descriptor des) {
		MethodElement element = DescriptorPropUtil.getDesciptorPropUtil().getLinkedElement(des);
		if (element == null) {
			return null;
		}
		return ConfigurationHelper.getCalculatedElement(element, getConfig());
	}	
	
	public Set<Descriptor> getAllReferenced() {
		return Collections.EMPTY_SET;
	}
	
	protected void addToSet(Set<Descriptor> set, List<? extends Descriptor> list) {
		if (list == null || list.isEmpty()) {
			return;
		}
		set.addAll(list);
	}
	
	public void updatePlainTextValues() {
		for (EStructuralFeature feature : featureSet) {
			getFeatureValue(feature);
		}
	}
	
}
