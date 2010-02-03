package org.eclipse.epf.library.realization;

import org.eclipse.epf.uma.MethodConfiguration;

public class RealizationContext {

	private MethodConfiguration config;
	private int mode = 0;
	
	public RealizationContext(MethodConfiguration config) {
		this(config, 0);
	}
	
	public RealizationContext(MethodConfiguration config, int mode) {
		this.config = config;
		this.mode = mode;
	}
	
	public MethodConfiguration getConfig() {
		return config;
	}
	
	public int getMode() {
		return mode;
	}
	
}
