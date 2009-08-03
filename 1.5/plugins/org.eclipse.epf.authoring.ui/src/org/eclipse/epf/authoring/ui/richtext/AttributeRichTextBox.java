/*******************************************************************************
 * Licensed Materials - Property of IBM
 * (c) Copyright IBM Corporation 2007-2008. All Rights Reserved.
 * 
 * Note to U.S. Government Users Restricted Rights:
 * Use, duplication or disclosure restricted by GSA ADP Schedule
 * Contract with IBM Corp. 
 *******************************************************************************/
package org.eclipse.epf.authoring.ui.richtext;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Phong Nguyen Le
 * @since 7.5
 *
 */
public class AttributeRichTextBox implements ModifyListener {
	public static final AttributeRichTextBox createAttributeRichTextBox(IMethodRichText richText, EObject object,
			EAttribute attr, boolean checkEditOnContainer,
			IActionManager actionMgr) {
//		IAttributeTextBoxFactory factory = AttributeTextBox.getAttributeTextBoxFactory();
//		if(factory != null) {
//			AttributeRichTextBox box = factory.createAttributeRichTextBox(richText, object, attr, checkEditOnContainer, actionMgr);
//			if(box != null) {
//				return box;
//			}
//		}
		return new AttributeRichTextBox(richText, object, attr, checkEditOnContainer, actionMgr);
	}
	
	protected EObject object;
	protected EAttribute attribute;
	private boolean checkEditOnContainer;
	private IActionManager actionMgr;
	protected IMethodRichText richText;
	
	protected AttributeRichTextBox(IMethodRichText richText, EObject object,
			EAttribute attr, boolean checkEditOnContainer,
			IActionManager actionMgr) {
		assert attr != null
				&& attr.getEAttributeType().getInstanceClass()
						.isAssignableFrom(String.class);
		this.object = object;
		this.attribute = attr;
		this.checkEditOnContainer = checkEditOnContainer;
		this.actionMgr = actionMgr;
		this.richText = richText;
		richText.addModifyListener(this);
	}
		
	public void setElement(EObject object) {
		if (object != this.object) {
			this.object = object;
			setAttribute();
		}
	}
	
	protected void setAttribute() {
		setText(getAttribute());
	}
	
	protected void setText(String text) {
		richText.removeModifyListener(this);
		try {
			richText.setText(text);
		}
		finally {
			richText.addModifyListener(this);
		}
	}
	
	protected String getAttribute() {
		Object val = object.eGet(attribute);
		return val == null ? "" : val.toString();
	}
	
	public IMethodRichText getRichText() {
		return richText;
	}

	public void modifyText(ModifyEvent e) {
		Shell shell = richText.getControl().getShell();
		IStatus status = TngUtil.checkEdit(object, shell);
		if(status.isOK()) {
			if(checkEditOnContainer && object.eContainer() != null) {
				status = TngUtil.checkEdit(object.eContainer(), shell);
			}
		}
		if(status.isOK()) {
			actionMgr.doAction(IActionManager.SET, object, attribute, richText.getText(), -1); 
		}
		else {
			// restore the text in the text widget
			//
			setAttribute();
			richText.setEditable(true);
		}
	}
	
}
