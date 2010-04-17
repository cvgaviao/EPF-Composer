package org.eclipse.epf.library.edit.uma;

import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;

public class MethodElementExt extends ExtendObject {

	private MethodElement element;
	public MethodElement getElement() {
		return element;
	}
	
	public MethodElementExt(MethodElement element) {
		this.element = element;
	}
		
}
