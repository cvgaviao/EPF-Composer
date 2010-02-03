package org.eclipse.epf.library.realization;

import org.eclipse.epf.library.realization.impl.RealizationManager;
import org.eclipse.epf.uma.MethodElement;

public interface IRealizationManager {
	
	IRealizedElement getRealizedElement(MethodElement element);
	IRealizedElement removeRealizedElement(MethodElement element);
	
}
