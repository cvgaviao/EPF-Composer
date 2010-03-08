package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.uma.MethodPlugin;


public class MethodPluginPropUtil extends MethodElementPropUtil {

	public static final String Plugin_SynFree = "plugin_synFree"; //$NON-NLS-1$

	private static MethodPluginPropUtil methodPluginPropUtil = new MethodPluginPropUtil();
	public static MethodPluginPropUtil getMethodPluginPropUtil() {
		return methodPluginPropUtil;
	}
	
	protected MethodPluginPropUtil() {		
	}
		
	public boolean isSynFree(MethodPlugin d) {
		Boolean value = getBooleanValue(d, Plugin_SynFree);
		return value == null ? false : value.booleanValue();
	}
	
	public void setSynFree(MethodPlugin d, boolean value) {
		setBooleanValue(d, Plugin_SynFree, value);
	}
	
	
}