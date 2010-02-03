package org.eclipse.epf.library.realization;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

public interface IRealizedElement {

	MethodElement getElement();
	
	Object getFeatureValue(EStructuralFeature feature);
	
	Object getOFeatureValue(OppositeFeature ofeature);
	
}
