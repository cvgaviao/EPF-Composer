package org.eclipse.epf.library.validation;

import org.eclipse.epf.common.utils.ExtensionHelper;
import org.eclipse.epf.library.configuration.ConfigurationData;
import org.eclipse.epf.library.edit.validation.IValidationManager;


public class ValidationManager implements IValidationManager {

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
	
	
}
