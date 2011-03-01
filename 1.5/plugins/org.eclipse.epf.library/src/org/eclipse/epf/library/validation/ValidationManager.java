package org.eclipse.epf.library.validation;

import org.eclipse.epf.common.utils.ExtensionHelper;
import org.eclipse.epf.library.configuration.ConfigurationData;
import org.eclipse.epf.library.edit.validation.IValidationManager;


public class ValidationManager implements IValidationManager {

	private boolean nameCheck = false;


	private boolean circularDependancyCheck = false;

	private boolean undeclaredDependancyCheck = false;

	protected ValidationManager() {		
	}
	
	private static ValidationManager instance;
	public static IValidationManager getInstance() {
		if (instance == null) {
			Object obj = ExtensionHelper.create(ValidationManager.class, null);
			if (obj instanceof ValidationManager) {
				instance = (ValidationManager) obj;
			}		
		}
		return instance;
	}
	

	public void setNameCheck(boolean b) {
		nameCheck = b;
	}

	public void setCircularDependancyCheck(boolean b) {
		circularDependancyCheck = b;
	}

	public void setUndeclaredDependancyCheck(boolean b) {
		undeclaredDependancyCheck = b;
	}
	
	public boolean isNameCheck() {
		return nameCheck;
	}
	
	public boolean isCircularDependancyCheck() {
		return circularDependancyCheck;
	}
		
	public boolean isUndeclaredDependancyCheck() {
		return undeclaredDependancyCheck;
	}
	
}
