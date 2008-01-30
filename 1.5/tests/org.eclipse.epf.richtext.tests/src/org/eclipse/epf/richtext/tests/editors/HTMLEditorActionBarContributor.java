//------------------------------------------------------------------------------
// Copyright (c) 2004, 2005 IBM Corporation.  All Rights Reserved.
//------------------------------------------------------------------------------
package org.eclipse.epf.richtext.tests.editors;

import org.eclipse.epf.richtext.tests.actions.BoldAction;
import org.eclipse.epf.richtext.tests.actions.ItalicAction;
import org.eclipse.epf.richtext.tests.actions.UnderlineAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.part.EditorActionBarContributor;

/**
 * A HTML editor action bar contributor.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class HTMLEditorActionBarContributor extends EditorActionBarContributor {
	
	private IEditorPart editor;
	private BoldAction boldAction;
	private ItalicAction italicAction;
	private UnderlineAction underlineAction;
	
	/**
	 * Creates a new instance.
	 */
	public HTMLEditorActionBarContributor() {
		super();
	}
	
	/**
	 * @see org.eclipse.ui.IEditorActionBarContributor#init(IActionBars, IWorkbenchPage)
	 */
    public void init(IActionBars bars, IWorkbenchPage page) {
    	super.init(bars, page);
    	boldAction = new BoldAction();
    	italicAction = new ItalicAction();
    	underlineAction = new UnderlineAction();
    	
    	IToolBarManager toolBar = bars.getToolBarManager();
    	
    	toolBar.add(boldAction);
    	toolBar.add(italicAction);
    	toolBar.add(underlineAction);
    	toolBar.add(new Separator());
    	
    }

	/**
	 * @see org.eclipse.ui.IEditorActionBarContributor#setActiveEditor(IEditorPart)
	 */
    public void setActiveEditor(IEditorPart activeEditor) {
    	editor = activeEditor;
    	boldAction.setActiveEditor(editor);
    	italicAction.setActiveEditor(editor);
    	underlineAction.setActiveEditor(editor);
    }

	/**
	 * @see org.eclipse.ui.IEditorActionBarContributor#setActiveEditor(IEditorPart)
	 */
    public void dispose() {
    }

}
