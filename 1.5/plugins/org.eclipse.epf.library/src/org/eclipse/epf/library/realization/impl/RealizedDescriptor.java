package org.eclipse.epf.library.realization.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.realization.IRealizedDescriptor;
import org.eclipse.epf.library.edit.realization.IRealizedElement;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.ContentElement;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public class RealizedDescriptor extends RealizedElement implements
		IRealizedDescriptor, IRealizedElement {

	private static Set<EStructuralFeature> featureSet = new HashSet<EStructuralFeature>();
	private static Map<EStructuralFeature, EStructuralFeature> contentFeatureMap = new HashMap<EStructuralFeature, EStructuralFeature>();

	static {
		UmaPackage up = UmaPackage.eINSTANCE;
		featureSet.add(up.getNamedElement_Name());
		featureSet.add(up.getMethodElement_PresentationName());
		featureSet.add(up.getMethodElement_BriefDescription());

		contentFeatureMap.put(up.getDescriptorDescription_RefinedDescription(),
				up.getContentDescription_MainDescription());
		contentFeatureMap.put(up.getContentDescription_KeyConsiderations(), up
				.getContentDescription_KeyConsiderations());		
		contentFeatureMap.put(up.getContentDescription_LongPresentationName(), up
				.getContentDescription_LongPresentationName());
	}
	
	public RealizedDescriptor(Descriptor descriptor) {
		super(descriptor);
	}
	
	public boolean handleFeature(EStructuralFeature feature) {
		return featureSet.contains(feature);
	}
	
	private Object getContentFeatureValue(EStructuralFeature feature) {
		EStructuralFeature elementFeature = contentFeatureMap.get(feature);
		
		if (elementFeature != null) {	

			Object value = null;
			if (getDescriptor().getPresentation() != null) {
				value = getDescriptor().getPresentation().eGet(feature);
			}
			
			if (getLinkedElement() == null || getLinkedElement().getPresentation() == null) {
				return value;
			}
			
			DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
			if (propUtil.isValueReplaced(feature, getDescriptor())) {
				return value;
			}
			
			Object linkedValue = ConfigurationHelper.calcAttributeFeatureValue(
					getLinkedElement().getPresentation(), elementFeature, getConfig());
			if (getDescriptor().getPresentation() != null && linkedValue != null && !linkedValue.equals(value)) {
				getDescriptor().getPresentation().eSet(feature, linkedValue);
			}

			return linkedValue;
		}
		
		throw new UnsupportedOperationException();
	}
	
	public Object getFeatureValue(EStructuralFeature feature) {
		if (contentFeatureMap.containsKey(feature)) {
			return getContentFeatureValue(feature);
		}
		
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
	
	protected ContentElement getLinkedElement() {
		MethodElement element = getRawLinkedElement();
		if (element == null) {
			return null;
		}
		return (ContentElement) ConfigurationHelper.getCalculatedElement(element, getConfig());
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
		
		Set<MethodElement> excludeElements = this.getExcludeRefSet(getDescriptor(), dFeature, realizer);
		
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
	
	public Set<Descriptor> updateAndGetAllReferenced() {
		List<Guidance> gList = getGuidances();
		return Collections.EMPTY_SET;
	}
	
	protected void addToSet(Set<Descriptor> set, List<? extends Descriptor> list) {
		if (list == null || list.isEmpty()) {
			return;
		}
		set.addAll(list);
	}
	
	public void updateStringValues() {
		for (EStructuralFeature feature : featureSet) {
			getFeatureValue(feature);
		}
		
		for (EStructuralFeature feature : contentFeatureMap.keySet()) {
			getFeatureValue(feature);
		}
	}
	
	private Set<MethodElement> getExcludeRefSet(Descriptor des,
			EReference ref, ElementRealizer realizer) {
		Set<MethodElement> set = new LinkedHashSet<MethodElement>();
		if (des == null) {
			return set;
		}
		try {
			EReference eRef = LibraryEditUtil.getInstance().getExcludeFeature(
					ref);
			Set<MethodElement> rawSet = getRawExcludeRefSet(des, ref, eRef,
					realizer.getConfiguration(), true);
			for (MethodElement elem : rawSet) {
				MethodElement realized = ConfigurationHelper.getCalculatedElement(elem, realizer);
				if (realized != null) {
					set.add(realized);
				}
			}
		} catch (Exception e) {
			LibraryPlugin.getDefault().getLogger().logError(e);
		}

		return set;
	}	

	private Set<MethodElement> getRawExcludeRefSet(Descriptor des,
			EReference ref, EReference eRef, MethodConfiguration config, boolean topLevelCall) {
		List<MethodElement> list;
		Set<MethodElement> refSet = new LinkedHashSet<MethodElement>();
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		Descriptor greenParent = propUtil.getGreenParentDescriptor(des);
		if (greenParent != null && ConfigurationHelper.inConfig(greenParent, config)) {
			Set<MethodElement> parentSet = getRawExcludeRefSet(greenParent,
					ref, eRef, config, false);
			refSet.addAll(parentSet);
			list = propUtil.getExcludeRefDeltaList(des, ref, false);
			if (list != null && !list.isEmpty()) {
				refSet.removeAll(list);
			}
			list = propUtil.getExcludeRefDeltaList(des, ref, true);
			if (list != null && !list.isEmpty()) {
				refSet.addAll(list);
			}
		}
		list = (List<MethodElement>) des.eGet(eRef);
		if (greenParent == null && list != null && ! list.isEmpty()) {
			refSet.addAll(list);
		}
		if (topLevelCall && greenParent != null) {
			if (list.size() != refSet.size()) {
				list.clear();
				list.addAll(refSet);
			}
		}
		return refSet;
	}

	public List<Guidance> getGuidances() {
		UmaPackage up = UmaPackage.eINSTANCE;
		List<Guidance> resultList = new ArrayList<Guidance>();

		Map<EReference, EReference> refMap = LibraryEditUtil.getInstance()
				.getGuidanceRefMap(getLinkedElementType());

		ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(getConfig());

		List<Guidance> excludeList = ConfigurationHelper.calc0nFeatureValue(
				getDescriptor(), up.getDescriptor_GuidanceExclude(), realizer);

		List<Guidance> addtionList = ConfigurationHelper.calc0nFeatureValue(
				getDescriptor(), up.getDescriptor_GuidanceAdditional(),
				realizer);

		for (Map.Entry<EReference, EReference> entry : refMap.entrySet()) {
			List<Guidance> subList = calculateGuidances(entry.getKey(), entry
					.getValue(), excludeList, addtionList);
			resultList.addAll(subList);
		}

		return resultList;
	}
	
	private List<Guidance> calculateGuidances(EReference eRef, EReference dRef,
			List<Guidance> excludeList, List<Guidance> addtionList) {
		UmaPackage up = UmaPackage.eINSTANCE;
		
		ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(getConfig());
		
		MethodElement element = getLinkedElement();
		if (element == null) {
			return ConfigurationHelper.calc0nFeatureValue(getDescriptor(),
					dRef, realizer);
		}
		
		List<Guidance> elementGuidanceList = ConfigurationHelper.calc0nFeatureValue(element,
				eRef, realizer);
		if (elementGuidanceList == null) {
			elementGuidanceList = new ArrayList<Guidance>();
		}

		if (!elementGuidanceList.isEmpty()) {
			if (excludeList != null && ! excludeList.isEmpty()) {
				elementGuidanceList.removeAll(excludeList);
			}
		} 
				
		boolean oldDeliver =  getDescriptor().eDeliver();		
		List<Guidance> valueList = (List<Guidance>) getDescriptor().eGet(dRef);
		try {
			getDescriptor().eSetDeliver(false);
			if (! valueList.isEmpty()) {
				valueList.clear();
			}
			if (!elementGuidanceList.isEmpty()) {
				valueList.addAll(elementGuidanceList);
			}
		} finally {
			getDescriptor().eSetDeliver(oldDeliver);
		}

		return valueList;
	}
	
	
	private EClass getLinkedElementType() {
		if (getDescriptor() instanceof TaskDescriptor) {
			return UmaPackage.eINSTANCE.getTask();
		}
		if (getDescriptor() instanceof RoleDescriptor) {
			return UmaPackage.eINSTANCE.getRole();
		}
		if (getDescriptor() instanceof WorkProductDescriptor) {
			return UmaPackage.eINSTANCE.getWorkProduct();
		}
	
		throw new UnsupportedOperationException();
	}
}
