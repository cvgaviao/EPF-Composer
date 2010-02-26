package org.eclipse.epf.library.edit.realization;

import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Process;

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
	
	/**
	 * Update process model with realization
	 */
	void updateModel(Process proc);
	
	void clearCacheData();
	
	boolean debug = false;
	
	boolean timing = false;
	
}
