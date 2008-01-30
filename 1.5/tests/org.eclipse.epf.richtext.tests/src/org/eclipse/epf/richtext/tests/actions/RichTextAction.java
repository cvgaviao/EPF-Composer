//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//--------------------------------------------------------------------------------
package org.eclipse.epf.richtext.tests.actions;

import org.eclipse.epf.richtext.tests.editors.HTMLEditor;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.IEditorPart;

/**
 * The abstract class for all rich text actions.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public abstract class RichTextAction extends Action {

	protected HTMLEditor editor = null;

	/**
	 * Creates a new instance.
	 */
	public RichTextAction() {
		super();
		init();
	}

	/**
	 * Initializes the action.
	 */
	public void init() {
	}

	/**
	 * Sets the active editor.
	 * 
	 * @param activeEditor
	 *            the active editor
	 */
	public void setActiveEditor(IEditorPart activeEditor) {
		if (activeEditor instanceof HTMLEditor) {
			editor = (HTMLEditor) activeEditor;
		}
	}

}