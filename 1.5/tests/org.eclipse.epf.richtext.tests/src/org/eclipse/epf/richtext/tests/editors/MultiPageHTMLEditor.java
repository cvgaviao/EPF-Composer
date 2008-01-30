//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//-------------------------------------------------------------------------------------
package org.eclipse.epf.richtext.tests.editors;

/**
 * A multi-page HTML editor built with a rich text control.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.epf.richtext.IRichText;
import org.eclipse.epf.richtext.RichText;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.MultiPageEditorPart;

public class MultiPageHTMLEditor extends MultiPageEditorPart {
	
	// The Rich Text editor.
	private IRichText richTextEditor;
	
	// The HTML source editor.
	private Text htmlEditor;
	
	/**
	 * Creates a new instance.
	 */
	public MultiPageHTMLEditor() {
		super();
	}
	
	/**
	 * @see org.eclipse.ui.part.EditorPart#init(IEditorSite, IEditorInput)
	 */
    public void init(IEditorSite site, IEditorInput input) throws PartInitException {
    	super.init(site, input);
	    setPartName(input.getName());	
    }
    
	/**
	 * @see org.eclipse.ui.part.MultiPageEditorPart#createPage()
	 */
    protected void createPages() {
    	createRichTextPage();
    	createSourcePage();
	}
	
    /**
     * Creates the Rich Text page.
     */
    protected void createRichTextPage() {
		richTextEditor = new RichText(getContainer(), SWT.NONE);
		int index = addPage(richTextEditor.getControl());
		setPageText(index, "Rich Text");
    }
    
    /**
     * Creates the HTML Source page.
     */
    protected void createSourcePage() {
    	htmlEditor = new Text(getContainer(), SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
		int index = addPage(htmlEditor);
		setPageText(index, "HTML");
    }    
    
	/**
	 * @see org.eclipse.ui.part.MultiPageEditorPart#pageChange(int)
	 */
	protected void pageChange(int newPageIndex) {
		super.pageChange(newPageIndex);
		switch (newPageIndex) {
			case 0:
				richTextEditor.setText(htmlEditor.getText());			
				break;
			case 1:
				htmlEditor.setText(richTextEditor.getText());			
				break;
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
    
}
