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
	
	/**
	 * Called at the beginning of publishing
	 */
	void beginPublish();
	
	/**
	 * Called at the end of publishing
	 */
	void endPublish();
	
	boolean debug = false;
	
	boolean timing = true;
	
}
