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

import org.eclipse.epf.richtext.IRichText;
import org.eclipse.epf.richtext.RichTextCommand;
import org.eclipse.jface.action.ControlContribution;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * Contributes a combo containing HTML font sizes to a tool bar.
 * 
 * @author Kelvin Low
 * @since  1.0
 */
public class FontSizeContribution extends ControlContribution {
	
	/**
	 * The Contribution ID.
	 */
	public static final String CONTRIBUTION_ID = FontSizeContribution.class.getName();
	
	// The rich text control.
	private IRichText richText;
	
	// The font size combo.
	private static Combo fontSizeCombo;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param	richText	The rich text control.
	 */
	public FontSizeContribution(IRichText richText) {
		super(CONTRIBUTION_ID);
		this.richText = richText;
	}
	
	/**
	 * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createControl(Composite parent) {
		fontSizeCombo = new Combo(parent, SWT.READ_ONLY | SWT.FLAT);
		fontSizeCombo.add("1"); //$NON-NLS-1$
		fontSizeCombo.add("2"); //$NON-NLS-1$
		fontSizeCombo.add("3"); //$NON-NLS-1$
		fontSizeCombo.add("4"); //$NON-NLS-1$
		fontSizeCombo.add("5"); //$NON-NLS-1$
		fontSizeCombo.add("6"); //$NON-NLS-1$
		fontSizeCombo.add("7"); //$NON-NLS-1$		
		fontSizeCombo.setVisibleItemCount(fontSizeCombo.getItemCount());
		fontSizeCombo.select(2);
		if (richText != null) {
			fontSizeCombo.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					Object eventSrc = event.getSource();
					if (eventSrc instanceof Combo) {
						Combo combo = (Combo)eventSrc;
						int fontSize = combo.getSelectionIndex() + 1;
						richText.executeCommand(RichTextCommand.SET_FONT_SIZE, "" + fontSize); //$NON-NLS-1$
					}
				};
			});
		}
		
		return fontSizeCombo;
	}
	
}