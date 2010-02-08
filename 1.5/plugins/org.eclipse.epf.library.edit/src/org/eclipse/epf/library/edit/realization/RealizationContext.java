package org.eclipse.epf.library.edit.realization;

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
	
	public String getId() {
		String modeStr = Integer.toString(mode);
		if (config == null) {
			return modeStr;
		}
		return config.getGuid() + modeStr;
	}
	
	@Override
	public String toString() {
		return "mode: " + mode + ", config: " + config; //$NON-NLS-1$ //$NON-NLS-2$
	}
	
}
