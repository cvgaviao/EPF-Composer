package org.eclipse.epf.library.realization.impl;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.realization.IRealizedElement;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public class RealizedElement implements IRealizedElement {

	private MethodElement element;
	private RealizationManager mgr;

	public RealizedElement(MethodElement element) {
		this.element = element;
	}
	
	public MethodElement getElement() {
		return element;
	}
	
	public String getName() {
		return (String) getFeatureValue(UmaPackage.eINSTANCE.getNamedElement_Name());
	}
	
	public String getPresentationName() {
		return (String) getFeatureValue(UmaPackage.eINSTANCE.getMethodElement_PresentationName());
	}
	
	public boolean handle(Object featureOrOFeature) {
		if (featureOrOFeature instanceof EStructuralFeature) {
			return handleFeature((EStructuralFeature) featureOrOFeature);
		}
		if (featureOrOFeature instanceof OppositeFeature) {
			return handleOFeature((OppositeFeature) featureOrOFeature);
		}
		
		return false;
	}
	
	public boolean handleFeature(EStructuralFeature feature) {
		return false;
	}
	
	public boolean handleOFeature(OppositeFeature ofeature) {
		return false;
	}
	
	public Object getFeatureValue(EStructuralFeature feature) {
		throw new UnsupportedOperationException();
	}
	
	public Object getOFeatureValue(OppositeFeature ofeature) {
		throw new UnsupportedOperationException();
	}
	
	protected MethodConfiguration getConfig() {
		return getMgr().getConfig();
	}
	
	public RealizationManager getMgr() {
		return mgr;
	}
	
	public void setMgr(RealizationManager mgr) {
		this.mgr = mgr;
	}
	
}
