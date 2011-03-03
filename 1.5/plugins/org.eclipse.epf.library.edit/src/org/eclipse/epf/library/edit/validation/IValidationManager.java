package org.eclipse.epf.library.edit.validation;

import org.eclipse.core.runtime.IProgressMonitor;

public interface IValidationManager {

	void setNameCheck(boolean b);
	boolean isNameCheck();

	void setCircularDependancyCheck(boolean b);
	boolean isCircularDependancyCheck();

	void setUndeclaredDependancyCheck(boolean b);
	
	void validate(Object scope, IProgressMonitor progressMonitor);
	
	
}
