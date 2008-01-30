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

import org.eclipse.epf.richtext.RichTextCommand;
import org.eclipse.epf.richtext.RichTextImages;
import org.eclipse.epf.richtext.RichTextResources;

/**
 * Toggles the 'bold' attribute of the selected text.
 * 
 * @author Kelvin Low
 * @since  1.0
 */
public class BoldAction extends RichTextAction {
	
	/**
	 * Creates a new instance.
	 */
	public BoldAction() {
		super();
	}
	
	/**
	 * Initializes the action.
	 */
	public void init() {
		setImageDescriptor(RichTextImages.IMG_DESC_BOLD);
		setToolTipText(RichTextResources.boldAction_toolTipText);	
	}
	
	public void run() {
		if (editor != null) {
			editor.executeCommand(RichTextCommand.BOLD, null);			
		}
		
	}

}
