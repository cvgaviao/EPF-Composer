//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//-------------------------------------------------------------------------------
package org.eclipse.epf.richtext.tests.actions;

import org.eclipse.epf.richtext.IRichText;
import org.eclipse.epf.richtext.RichTextCommand;
import org.eclipse.epf.richtext.html.BlockTag;
import org.eclipse.jface.action.ControlContribution;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * Contributes a combo containing HTML block tags to a tool bar.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class BlockTagContribution extends ControlContribution {

	/**
	 * The Contribution ID.
	 */
	public static final String CONTRIBUTION_ID = BlockTagContribution.class
			.getName();

	// The rich text control.
	private IRichText richText;

	// The block tag combo.
	private static Combo blockTagCombo;

	/**
	 * Creates a new instance.
	 * 
	 * @param richText
	 *            The rich text control.
	 */
	public BlockTagContribution(IRichText richText) {
		super(CONTRIBUTION_ID);
		this.richText = richText;
	}

	/**
	 * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createControl(Composite parent) {
		blockTagCombo = new Combo(parent, SWT.READ_ONLY | SWT.FLAT);
		blockTagCombo.add(BlockTag.PARAGRAPH.getName());
		blockTagCombo.add(BlockTag.HEADING_1.getName());
		blockTagCombo.add(BlockTag.HEADING_2.getName());
		blockTagCombo.add(BlockTag.HEADING_3.getName());
		blockTagCombo.add(BlockTag.HEADING_4.getName());
		blockTagCombo.add(BlockTag.HEADING_5.getName());
		blockTagCombo.add(BlockTag.HEADING_6.getName());
		blockTagCombo.add(BlockTag.ADDRESS.getName());
		blockTagCombo.add(BlockTag.PREFORMATTED_TEXT.getName());
		blockTagCombo.setVisibleItemCount(blockTagCombo.getItemCount());
		blockTagCombo.select(0);
		if (richText != null) {
			blockTagCombo.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					Object eventSrc = event.getSource();
					if (eventSrc instanceof Combo) {
						Combo combo = (Combo) eventSrc;
						int index = combo.getSelectionIndex();
						String value = BlockTag.getBlockTag(index).getValue();
						richText.executeCommand(RichTextCommand.FORMAT_BLOCK,
								value);
					}
				};
			});
		}

		return blockTagCombo;
	}

}