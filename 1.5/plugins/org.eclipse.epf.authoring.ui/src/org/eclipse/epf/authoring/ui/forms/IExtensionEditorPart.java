//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.authoring.ui.forms;

import org.eclipse.ui.forms.editor.FormEditor;

/**
 * Interface to be used for all pages added through extension points
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public interface IExtensionEditorPart {

	/**
	 * Return a Control, IFormPage, or IEditorPart to be added
	 * @param editor
	 * @param input TODO
	 */
	public Object getContribution(FormEditor editor, Object input);
		
	/**
	 * 
	 * @return name of part, if the part is a IFormPage this can be null
	 */
	public String getPartName();
	
	/**
	 * 
	 * @return true if extension should contribute to this object's editor
	 */
	public boolean isValid(Object object);
}
