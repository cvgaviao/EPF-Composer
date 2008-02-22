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

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.forms.editor.FormEditor;

/**
 * Interface to be used for all pages added through extension points
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public interface IExtensionEditorPart   {

	/**
	 * Set editor for form page
	 * @param editor
	 */
	public IEditorPart setEditor(FormEditor editor) ;
	
	/**
	 * Set input for form page
	 * @param input
	 */
	public void setInput(Object input);
	
	/**
	 * 
	 * @return name of part, if the part is a FormPage this can be null
	 */
	public String getPartName();
}
