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
package org.eclipse.epf.authoring.ui.forms;

import org.eclipse.epf.authoring.ui.editors.MethodRichText;
import org.eclipse.epf.authoring.ui.editors.MethodRichTextEditor;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichText;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichTextEditor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.forms.widgets.FormToolkit;


/**
 * A help class for creating UI widgets adapted adapted to work in Eclipse
 * forms.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class MethodFormToolkit {

	/**
	 * Creates a Rich Text control and adapts it to be used in a form.
	 * 
	 * @param parent
	 *            The parent control.
	 * @param text
	 *            The initial text for the viewer.
	 * @param style
	 *            The initial style for the viewer.
	 * @param basePath
	 *            The base path used for resolving hrefs.
	 * @return A new <code>IMethodRichText</code> instance.
	 */
	public static IMethodRichText createRichText(FormToolkit toolkit,
			Composite parent, String text, int style, String basePath,
			MethodElement methodElement, Label label) {
		IMethodRichText richText = new MethodRichText(parent, style, basePath);
		richText.init(methodElement, label);
		richText.getControl().setData(FormToolkit.KEY_DRAW_BORDER,
				FormToolkit.TEXT_BORDER);
		if (text != null) {
			richText.setText(text);
		}
		return richText;
	}

	/**
	 * Creates a Rich Text editor and adapts it to be used in a form.
	 * 
	 * @param parent
	 *            The parent control.
	 * @param text
	 *            The initial text for the viewer.
	 * @param style
	 *            The initial style for the viewer.
	 * @param basePath
	 *            The base path used for resolving hrefs.
	 * @return A new <code>IMethodRichTextEditor</code> instance.
	 */
	public static IMethodRichTextEditor createRichTextEditor(
			FormToolkit toolkit, Composite parent, String text, int style,
			String basePath, MethodElement methodElement, Label label, IEditorSite editorSite) {
		IMethodRichTextEditor editor = new MethodRichTextEditor(parent, style,
				basePath, methodElement, label, editorSite);
		editor.getControl().setData(FormToolkit.KEY_DRAW_BORDER,
				FormToolkit.TEXT_BORDER);
		if (text != null) {
			editor.setText(text);
		}
		return editor;
	}

}
