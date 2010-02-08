package org.eclipse.epf.library.edit.realization;

import org.eclipse.epf.uma.MethodElement;

public interface IRealizationManager {
	
	/**
	 * @param element
	 * @return
	 */
	IRealizedElement getRealizedElement(MethodElement element);
	
	/**
	 * @param element
	 * @return
	 */
	IRealizedElement removeRealizedElement(MethodElement element);
	
	void clearCacheData();
	
	boolean test = false;
	
	boolean debug = false;
	
}
