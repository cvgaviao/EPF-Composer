package org.eclipse.epf.common.utils;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.IStatus;

public class ValidateEdit {

	public ValidateEdit() {		
	}
	
	public IStatus validateEdit(IWorkspace workspace, IFile[] files, Object context) {
		return workspace.validateEdit(files, context);
	}
	
}
