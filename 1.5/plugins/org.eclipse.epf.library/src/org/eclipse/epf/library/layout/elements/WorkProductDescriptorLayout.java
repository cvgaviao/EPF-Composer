//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.layout.elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.layout.ElementLayoutManager;
import org.eclipse.epf.library.layout.util.XmlElement;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.util.AssociationHelper;


public class WorkProductDescriptorLayout extends DescriptorLayout {
	
	public static final Collection<EStructuralFeature> extraFeaturesFromWorkProduct = Arrays.asList(new EStructuralFeature[] {
			UmaPackage.eINSTANCE.getWorkProduct_EstimationConsiderations(),
			UmaPackage.eINSTANCE.getWorkProduct_Reports(),
			UmaPackage.eINSTANCE.getWorkProduct_Templates(),
			UmaPackage.eINSTANCE.getWorkProduct_ToolMentors()
	});

	public void init(ElementLayoutManager layoutManager, MethodElement element) {
		super.__init(layoutManager, element);
	}

	/**
	 * @see org.eclipse.epf.library.layout.elements.AbstractElementLayout#loadReferences(XmlElement, boolean)
	 */
	public void loadReferences(XmlElement elementXml, boolean includeReferences) {

		super.loadReferences(elementXml, includeReferences);
		
		// 161325 - WPDescriptors did not show modify RoleDescriptors
		// get the modifies feature
		// this guy does not have a defined opposite feature
		// this method call is not safe. 
//		List modifyRoles = ConfigurationHelper.calcModifyRoleDescriptors(
//				(WorkProductDescriptor)super.element, 
//				layoutManager.getElementRealizer());
		
		List modifyRoles = calcModifyRoleDescriptors();
		
		addReferences(null, elementXml, "workedOnBy", modifyRoles); //$NON-NLS-1$
		
		elementXml.setAttribute("ShowFullMethodContent", (layoutManager.getValidator().showExtraInfoForDescriptors()) ? "true" : "false"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		
		if ((super.elementLayout != null ) && layoutManager.getValidator().showExtraInfoForDescriptors() ) {
			// also load the linked element referenced information
			for (Iterator<EStructuralFeature> iterator = getExtraFeaturesFromContentElement().iterator(); iterator.hasNext();) {
				EStructuralFeature feature = iterator.next();
				super.elementLayout.loadFeature(feature, elementXml, false);
			}

		}
	}
	
	@Override
	protected Collection<EStructuralFeature> getExtraFeaturesFromContentElement() {
		ArrayList<EStructuralFeature> features = new ArrayList<EStructuralFeature>(extraFeaturesFromWorkProduct);
		features.addAll(super.getExtraFeaturesFromContentElement());
		return features;
	}
	
	public List calcModifyRoleDescriptors() {
		
		List modifyRoles = new ArrayList();

		ElementRealizer realizer = getLayoutMgr().getElementRealizer();
		List taskDescriptors = ConfigurationHelper.calc0nFeatureValue(
				element, 
				AssociationHelper.WorkProductDescriptor_OutputFrom_TaskDescriptors, 
				realizer);
		
		EStructuralFeature feature = UmaPackage.eINSTANCE.getTaskDescriptor_Output();
		for (Iterator it = taskDescriptors.iterator(); it.hasNext(); ) {
			TaskDescriptor t = (TaskDescriptor)it.next();
			
			// make sure the task descriptor has the current wp descriptor as output
			String path = super.makePath(getSuperActivityPath(), t);
			TaskDescriptorLayout tl = (TaskDescriptorLayout)layoutManager.createLayout(t, owningProcess, path);
			List outputs = (List)tl.getFeatureValue(feature, null, false);
			
			if ( outputs != null && outputs.contains(element) ) {	
				List roleDescriptors = (List) tl.getFeatureValue(UmaPackage.eINSTANCE.getTaskDescriptor_PerformedPrimarilyBy(), null, false);
				if (roleDescriptors != null) {
				for (Iterator i = roleDescriptors.iterator(); i.hasNext();)
				{
					RoleDescriptor r = (RoleDescriptor) i.next();
					if ( (r != null) && !modifyRoles.contains(r) ) {
						modifyRoles.add(r);
					}
				}
				}
			}
		}
		
		return modifyRoles;
	}
	
	protected boolean acceptFeatureValue(OppositeFeature feature, Object value) {
		
		if ( !super.acceptFeatureValue(feature, value) ) {
			return false;
		}
		
		if ( !isTaskOppositeFeature(feature) || !(value instanceof List) ) {
			return true;
		}
		
		List items = (List)value;
		int i = 0;
		while (i < items.size() ) {
			MethodElement e = (MethodElement)items.get(i);
			String path = super.makePath(getSuperActivityPath(), e);
			TaskDescriptorLayout tl = (TaskDescriptorLayout)layoutManager.createLayout(e, owningProcess, path);
			List o = null;
			if ( feature == AssociationHelper.WorkProductDescriptor_ExternalInputTo_TaskDescriptors ) {
				o = (List)tl.getFeatureValue(UmaPackage.eINSTANCE.getTaskDescriptor_ExternalInput(), null, false);			
			} else if ( feature == AssociationHelper.WorkProductDescriptor_MandatoryInputTo_TaskDescriptors ) {
				o = (List)tl.getFeatureValue(UmaPackage.eINSTANCE.getTaskDescriptor_MandatoryInput(), null, false);			
			} else if ( feature == AssociationHelper.WorkProductDescriptor_OptionalInputTo_TaskDescriptors ) {
				o = (List)tl.getFeatureValue(UmaPackage.eINSTANCE.getTaskDescriptor_OptionalInput(), null, false);			
			} else if ( feature == AssociationHelper.WorkProductDescriptor_OutputFrom_TaskDescriptors ) {
				o = (List)tl.getFeatureValue(UmaPackage.eINSTANCE.getTaskDescriptor_Output(), null, false);			
			} 
			
			if ( (o != null) && o.contains(this.element) ) {
				i++;
			} else {
				items.remove(i);
			}
		}


		
		return true;
	}
	
}
