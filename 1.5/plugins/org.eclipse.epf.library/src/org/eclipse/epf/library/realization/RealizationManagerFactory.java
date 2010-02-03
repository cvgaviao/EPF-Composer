package org.eclipse.epf.library.realization;

import org.eclipse.epf.library.realization.impl.RealizationManager;

public class RealizationManagerFactory {
	
	public static RealizationManagerFactory instance = new RealizationManagerFactory();

	public static RealizationManagerFactory getInstance() {
		return instance;
	}

	public RealizationManagerFactory() {		
	}
	
	public IRealizationManager getRealizationManger(RealizationContext context) {
		return new RealizationManager(context);
	}
	
	
}
