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
