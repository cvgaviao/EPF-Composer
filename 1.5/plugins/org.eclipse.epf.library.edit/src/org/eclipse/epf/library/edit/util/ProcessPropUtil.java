package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.library.edit.uma.Scope;
import org.eclipse.epf.uma.Process;


public class ProcessPropUtil extends MethodElementPropUtil {

	public static final String Process_SynFree = "process_synFree"; //$NON-NLS-1$
	
	//Volatile object name strings
	public static final String V_PROCESS_Scope = "v_process_scope";			//$NON-NLS-1$

	private static ProcessPropUtil processPropUtil = new ProcessPropUtil();
	public static ProcessPropUtil getProcessPropUtil() {
		return processPropUtil;
	}
	
	protected ProcessPropUtil() {		
	}
	
	public boolean isSynFree(Process d) {
		Boolean value = getBooleanValue(d, Process_SynFree);
		return value == null ? false : value.booleanValue();
	}
	
	public void setSynFree(Process d, boolean value) {
		setBooleanValue(d, Process_SynFree, value);
	}
	
	public Scope getScope(Process proc) {
		return (Scope) getValitileObject(proc, V_PROCESS_Scope);
	}
	
	public void setScope(Process proc, Scope scope) {
		storeValitileObject(proc, V_PROCESS_Scope, scope);
	}
	
	
}