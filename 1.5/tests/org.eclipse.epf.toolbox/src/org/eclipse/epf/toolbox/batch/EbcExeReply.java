package org.eclipse.epf.toolbox.batch;

import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.epf.toolbox.ToolboxPlugin;


public class EbcExeReply {
	
	private static final String pluginId = " ";

	private MultiStatus mstatus = new MultiStatus(pluginId, 0, null, null);
	
	public EbcExeReply() {		
	}
	
	public boolean isOk() {
		return mstatus.isOK();
	}
	
	public String toString() {
		return mstatus.toString();
	}
	
	public void addStatus(int severity, String message, Throwable exception) {
		Status status = new Status(severity, pluginId, 0, message, exception);
		mstatus.add(status);
	}		
	
}
