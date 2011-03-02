package org.eclipse.epf.library.validation;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.epf.common.utils.ExtensionHelper;
import org.eclipse.epf.library.edit.validation.IValidationManager;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.validation.LibraryEValidator;

public class ValidationManager implements IValidationManager {

	private boolean nameCheck = false;

	private boolean circularDependancyCheck = false;

	private boolean undeclaredDependancyCheck = false;
	
	private DiagnosticChain diagnostics;

	private IProgressMonitor progressMonitor;

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
	
	public DiagnosticChain getDiagnostics() {
		return diagnostics;
	}
	
	public IProgressMonitor getProgressMonitor() {
		return progressMonitor;
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
		
	public boolean isUndeclaredDepenancyCheck() {
		return undeclaredDependancyCheck;
	}
	
	public void validate(DiagnosticChain diagnostics, Object scope, IProgressMonitor progressMonitor)  {
		this.diagnostics = diagnostics;
		this.progressMonitor = progressMonitor;
		initValidationScope(scope);		
		try {
			validate();
		} finally {
			this.diagnostics = null;
			this.progressMonitor = null;
			
			pluginSet = null;
		}
	}
	
	
	private Set<MethodPlugin> pluginSet;
	public Set<MethodPlugin> getPluginSet() {
		return pluginSet;
	}

	protected void initValidationScope(Object scope) {
		pluginSet = new LinkedHashSet<MethodPlugin>();
		if (scope instanceof MethodLibrary) {
			pluginSet.addAll(((MethodLibrary) scope).getMethodPlugins());
		} else if (scope instanceof List) {
			for (Object obj : (List) scope) {
				if (obj instanceof MethodPlugin) {
					pluginSet.add((MethodPlugin) obj);
				}
			}
		}
	}
	
	protected void validate()  {
		if (isUndeclaredDepenancyCheck()) {
			UndeclaredDependencyCheck check = newUndeclaredDependencyCheck();
			check.run();
		}
	}
	
	private void appendDiagnostics(IStatus status, DiagnosticChain diagnostics) {
		LibraryEValidator.appendDiagnostics(status, diagnostics);
	}
	
	protected UndeclaredDependencyCheck newUndeclaredDependencyCheck() {
		return new UndeclaredDependencyCheck(this);
	}
	
}
