//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//----------------------------------------------------------------------------------
package org.eclipse.epf.richtext.tests.editors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.epf.richtext.IRichText;
import org.eclipse.epf.richtext.RichText;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

/**
 * A HTML editor built with a rich text.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class HTMLEditor extends EditorPart {

	// The Rich Text control.
	private IRichText richText;

	/**
	 * Creates a new instance.
	 */
	public HTMLEditor() {
		super();
	}

	/**
	 * @see org.eclipse.ui.part.EditorPart#init(IEditorSite, IEditorInput)
	 */
	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		try {
			setSite(site);
			setInput(input);
			setPartName(input.getName());
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	/**
	 * @see org.eclipse.ui.part.EditorPart#createPartControl(Composite)
	 */
	public void createPartControl(Composite parent) {
		try {
			richText = new RichText(parent, SWT.NONE);
		} catch (Exception e) {
			richText = null;
			e.printStackTrace();
		}
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPart#setFocus()
	 */
	public void setFocus() {
		if (richText != null) {
			richText.setFocus();
		}
	}

	/**
	 * @see org.eclipse.ui.part.EditorPart#disDirty()
	 */
	public boolean isDirty() {
		return false;
	}

	/**
	 * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
	 */
	public boolean isSaveAsAllowed() {
		return false;
	}

	/**
	 * @see org.eclipse.ui.part.EditorPart#doSave(IProgressMonitor)
	 */
	public void doSave(IProgressMonitor monitor) {
	}

	/**
	 * @see org.eclipse.ui.part.EditorPart#doSaveAs()
	 */
	public void doSaveAs() {
	}

	/**
	 * Executes a rich text command.
	 * 
	 * @param command
	 *            a rich text command string
	 * @param params
	 *            a comma-delimited commands assciated with the command
	 */
	public void executeCommand(String command, String params) {
		if (richText != null) {
			richText.executeCommand(command, params);
		}
	}

}
