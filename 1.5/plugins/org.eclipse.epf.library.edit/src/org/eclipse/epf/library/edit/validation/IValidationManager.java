package org.eclipse.epf.library.edit.validation;

import java.util.List;

import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.ecore.EObject;

public interface IValidationManager {

	void setNameCheck(boolean b);

	void setCircularDependancyCheck(boolean b);

	void setUndeclaredDependancyCheck(boolean b);
	
	void validate(DiagnosticChain diagnostics, Object scope);
	
	
}
