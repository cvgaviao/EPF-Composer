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
package org.eclipse.epf.richtext.tests.actions;

import org.eclipse.epf.richtext.IRichText;
import org.eclipse.epf.richtext.RichTextCommand;
import org.eclipse.epf.richtext.html.FontName;
import org.eclipse.jface.action.ControlContribution;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * Contributes a combo containing HTML font names to a tool bar.
 * 
 * @author Kelvin Low
 * @since  1.0
 */
public class FontNameContribution extends ControlContribution {
	
	/**
	 * The Contribution ID.
	 */
	public static final String CONTRIBUTION_ID = FontNameContribution.class.getName();
	
	// The rich text control.
	private IRichText richText;
	
	// The font name combo.
	private static Combo fontNameCombo;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param	richText	The rich text control.
	 */
	public FontNameContribution(IRichText richText) {
		super(CONTRIBUTION_ID);
		this.richText = richText;
	}
	
	/**
	 * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createControl(Composite parent) {
		fontNameCombo = new Combo(parent, SWT.READ_ONLY | SWT.FLAT);
		fontNameCombo.add(FontName.DEFAULT.getName());
		fontNameCombo.add(FontName.ARIAL.getName());
		fontNameCombo.add(FontName.COURIER_NEW.getName());
		fontNameCombo.add(FontName.TIMES_NEW_ROMAN.getName());
		fontNameCombo.add(FontName.VERDANA.getName());
		fontNameCombo.setVisibleItemCount(fontNameCombo.getItemCount());		
		fontNameCombo.select(0);
		if (richText != null) {
			fontNameCombo.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					Object eventSrc = event.getSource();
					if (eventSrc instanceof Combo) {
						Combo combo = (Combo)eventSrc;
						int index = combo.getSelectionIndex();
						String value = FontName.getFontName(index).getValue();
						richText.executeCommand(RichTextCommand.SET_FONT_NAME, value);
					}
				};
			});
		}
		
		return fontNameCombo;
	}
	
}