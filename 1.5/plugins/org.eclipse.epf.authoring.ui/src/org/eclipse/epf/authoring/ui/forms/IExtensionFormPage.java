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
import org.eclipse.ui.forms.editor.IFormPage;

/**
 * Interface to be used for all pages added through extension points
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public interface IExtensionFormPage   {

	/**
	 * Set editor for form page
	 * @param editor
	 */
	public IFormPage setEditor(FormEditor editor) ;
	
	/**
	 * Set input for form page
	 * @param input
	 */
	public void setInput(Object input);
}
