//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//--------------------------------------------------------------------------------
package org.eclipse.epf.richtext.tests.editors;

import org.eclipse.epf.richtext.IRichTextToolBar;
import org.eclipse.epf.richtext.RichTextEditor;
import org.eclipse.epf.richtext.actions.AddImageAction;
import org.eclipse.epf.richtext.actions.AddLineAction;
import org.eclipse.epf.richtext.actions.AddLinkAction;
import org.eclipse.epf.richtext.actions.AddOrderedListAction;
import org.eclipse.epf.richtext.actions.AddTableAction;
import org.eclipse.epf.richtext.actions.AddUnorderedListAction;
import org.eclipse.epf.richtext.actions.BlockTagAction;
import org.eclipse.epf.richtext.actions.BoldAction;
import org.eclipse.epf.richtext.actions.CopyAction;
import org.eclipse.epf.richtext.actions.CutAction;
import org.eclipse.epf.richtext.actions.FontNameAction;
import org.eclipse.epf.richtext.actions.FontSizeAction;
import org.eclipse.epf.richtext.actions.FontStyleAction;
import org.eclipse.epf.richtext.actions.IndentAction;
import org.eclipse.epf.richtext.actions.ItalicAction;
import org.eclipse.epf.richtext.actions.JustifyCenterAction;
import org.eclipse.epf.richtext.actions.JustifyFullAction;
import org.eclipse.epf.richtext.actions.JustifyLeftAction;
import org.eclipse.epf.richtext.actions.JustifyRightAction;
import org.eclipse.epf.richtext.actions.OutdentAction;
import org.eclipse.epf.richtext.actions.PasteAction;
import org.eclipse.epf.richtext.actions.SubscriptAction;
import org.eclipse.epf.richtext.actions.SuperscriptAction;
import org.eclipse.epf.richtext.actions.TidyActionGroup;
import org.eclipse.epf.richtext.actions.UnderlineAction;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorSite;

/**
 * Extends the base rich text editor and populates its tool bar with some useful
 * action items.
 * 
 * @author Kelvin Low
 * @author Jeff Hardy
 * @since 1.0
 */
public class RichTextTestEditor extends RichTextEditor {

	/**
	 * Creates a new instance.
	 * 
	 * @param parent
	 *            The parent composite.
	 * @param style
	 *            The style for the control.
	 * @param basePath
	 *            The base path used for resolving hyperlinks in the editor
	 *            content.
	 */
	public RichTextTestEditor(Composite parent, int style, IEditorSite editorSite) {
		super(parent, style, editorSite);
	}

	/**
	 * Fills the Rich Text editor tool bar with action items.
	 * 
	 * @param toolBar
	 *            The Rich text editor tool bar.
	 */
	public void fillToolBar(IRichTextToolBar toolBar) {
/*		toolBar.addAction(new FontStyleAction(this));
		toolBar.addAction(new BlockTagAction());
		toolBar.addAction(new FontNameAction(this));
		toolBar.addAction(new FontSizeAction(this));
		toolBar.addAction(new CutAction(this));
		toolBar.addAction(new CopyAction(this));
		toolBar.addAction(new PasteAction(this));
		toolBar.addSeparator();
		toolBar.addAction(new BoldAction(this));
		toolBar.addAction(new ItalicAction(this));
		toolBar.addAction(new UnderlineAction(this));
		toolBar.addSeparator();
		toolBar.addAction(new SubscriptAction(this));
		toolBar.addAction(new SuperscriptAction(this));
		toolBar.addSeparator();
		toolBar.addAction(new TidyActionGroup(this));
		toolBar.addSeparator();
		toolBar.addAction(new JustifyLeftAction(this));
		toolBar.addAction(new JustifyCenterAction(this));
		toolBar.addAction(new JustifyRightAction(this));
		toolBar.addAction(new JustifyFullAction(this));
		toolBar.addSeparator();
		toolBar.addAction(new AddOrderedListAction(this));
		toolBar.addAction(new AddUnorderedListAction(this));
		toolBar.addSeparator();
		toolBar.addAction(new OutdentAction(this));
		toolBar.addAction(new IndentAction(this));
		toolBar.addSeparator();
		if (richText.getFindReplaceAction() != null)
			toolBar.addAction(richText.getFindReplaceAction());
		toolBar.addSeparator();
		toolBar.addAction(new AddLineAction(this));
		toolBar.addAction(new AddLinkAction(this));
		toolBar.addAction(new AddImageAction(this));
		toolBar.addAction(new AddTableAction(this));*/
	}

}
