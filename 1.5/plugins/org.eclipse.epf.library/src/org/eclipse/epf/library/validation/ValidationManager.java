package org.eclipse.epf.library.validation;

import java.util.ArrayList;
import java.util.List;

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
	
	private DiagnosticChain getDiagnostics() {
		return diagnostics;
	}
	
	private IProgressMonitor getProgressMonitor() {
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
			
			pluginList = null;
		}
	}
	
	
	private List<MethodPlugin> pluginList;
	protected List<MethodPlugin> getPluginList() {
		return pluginList;
	}

	protected void initValidationScope(Object scope) {
		pluginList = new ArrayList<MethodPlugin>();
		if (scope instanceof MethodLibrary) {
			pluginList.addAll(((MethodLibrary) scope).getMethodPlugins());
		} else if (scope instanceof List) {
			for (Object obj : (List) scope) {
				if (obj instanceof MethodPlugin) {
					pluginList.add((MethodPlugin) obj);
				}
			}
		}
	}
	
	protected void validate()  {
		
	}
	
	private void appendDiagnostics(IStatus status, DiagnosticChain diagnostics) {
		LibraryEValidator.appendDiagnostics(status, diagnostics);
	}
	
}
