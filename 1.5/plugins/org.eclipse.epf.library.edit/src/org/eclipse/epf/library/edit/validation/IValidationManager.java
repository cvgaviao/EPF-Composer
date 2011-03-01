package org.eclipse.epf.library.edit.validation;

public interface IValidationManager {

	void setNameCheck(boolean b);

	void setCircularDependancyCheck(boolean b);

	void setUndeclaredDependancyCheck(boolean b);
	
	
}
