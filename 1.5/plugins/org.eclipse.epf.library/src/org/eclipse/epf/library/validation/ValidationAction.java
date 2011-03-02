package org.eclipse.epf.library.validation;

public class ValidationAction {

	private ValidationManager mgr;
	
	protected ValidationManager getMgr() {
		return mgr;
	}
	
	protected void setMgr(ValidationManager mgr) {
		this.mgr = mgr;
	}
	
	public ValidationAction(ValidationManager mgr) {
		this.mgr = mgr;
	}
	
	public void run() {
		
	}
	
}
