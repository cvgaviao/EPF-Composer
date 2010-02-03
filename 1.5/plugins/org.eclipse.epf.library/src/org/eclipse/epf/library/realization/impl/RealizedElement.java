package org.eclipse.epf.library.realization.impl;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.realization.IRealizedElement;
import org.eclipse.epf.library.realization.RealizationContext;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public class RealizedElement implements IRealizedElement {

	private MethodElement element;
	private RealizationManager mgr;

	private MethodConfiguration config;

	public RealizedElement(MethodElement element) {
		this.element = element;
	}
	
	public MethodElement getElement() {
		return element;
	}
	
	public Object getFeatureValue(EStructuralFeature feature) {
		return null;
	}
	
	public Object getOFeatureValue(OppositeFeature ofeature) {
		return null;
	}
	
	public MethodConfiguration getConfig() {
		return config;
	}

	public void setConfig(MethodConfiguration config) {
		this.config = config;
	}
	
	public RealizationManager getMgr() {
		return mgr;
	}
	
	public void setMgr(RealizationManager mgr) {
		this.mgr = mgr;
	}
	
}
