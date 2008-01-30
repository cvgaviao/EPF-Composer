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
package org.eclipse.epf.richtext.tests.views;

import org.eclipse.epf.richtext.IRichText;
import org.eclipse.epf.richtext.RichText;
import org.eclipse.epf.richtext.RichTextCommand;
import org.eclipse.epf.richtext.RichTextImages;
import org.eclipse.epf.richtext.dialogs.AddImageDialog;
import org.eclipse.epf.richtext.dialogs.AddLinkDialog;
import org.eclipse.epf.richtext.dialogs.AddTableDialog;
import org.eclipse.epf.richtext.html.Image;
import org.eclipse.epf.richtext.html.Link;
import org.eclipse.epf.richtext.html.Table;
import org.eclipse.epf.richtext.tests.actions.BlockTagContribution;
import org.eclipse.epf.richtext.tests.actions.FontNameContribution;
import org.eclipse.epf.richtext.tests.actions.FontSizeContribution;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;

/**
 * A test view containing a rich text control.
 * 
 * @author Kelvin Low
 * @since  1.0
 */
public class RichTextTestView extends ViewPart {
	
	private IRichText richText;
	
	/**
	 * Creates a new instance.
	 */
	public RichTextTestView() {
		super();
	}
	
	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(Composite)
	 */
	public void createPartControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());
		
		richText = new RichText(composite, SWT.NONE);
		if (richText == null) {
			return;
		}
		
		richText.setText("<h3>Hello World</h3>");
		
		Action boldAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.BOLD);
			}
		};
		boldAction.setImageDescriptor(RichTextImages.IMG_DESC_BOLD);
		
		Action italicAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.ITALIC);
			}
		};
		italicAction.setImageDescriptor(RichTextImages.IMG_DESC_ITALIC);		
		
		Action underlineAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.UNDERLINE);
			}
		};
		underlineAction.setImageDescriptor(RichTextImages.IMG_DESC_UNDERLINE);
		
		Action subscriptAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.SUBSCRIPT);
			}
		};
		subscriptAction.setImageDescriptor(RichTextImages.IMG_DESC_SUBSCRIPT);
		
		Action superscriptAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.SUPERSCRIPT);
			}
		};
		superscriptAction.setImageDescriptor(RichTextImages.IMG_DESC_SUPERSCRIPT);
		
		Action justifyLeftAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.JUSTIFY_LEFT);
			}
		};
		justifyLeftAction.setImageDescriptor(RichTextImages.IMG_DESC_JUSTIFY_LEFT);
		
		Action justifyCenterAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.JUSTIFY_CENTER);
			}
		};
		justifyCenterAction.setImageDescriptor(RichTextImages.IMG_DESC_JUSTIFY_CENTER);
		
		Action justifyRightAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.JUSTIFY_RIGHT);
			}
		};
		justifyRightAction.setImageDescriptor(RichTextImages.IMG_DESC_JUSTIFY_RIGHT);
		
		Action justifyFullAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.JUSTIFY_FULL);
			}
		};
		justifyFullAction.setImageDescriptor(RichTextImages.IMG_DESC_JUSTIFY_FULL);		
		
		Action addOrderedListAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.ADD_ORDERED_LIST);
			}
		};
		addOrderedListAction.setImageDescriptor(RichTextImages.IMG_DESC_ADD_ORDERED_LIST);
		
		Action addUnorderedListAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.ADD_UNORDERED_LIST);
			}
		};
		addUnorderedListAction.setImageDescriptor(RichTextImages.IMG_DESC_ADD_UNORDERED_LIST);
		
		Action outdentAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.OUTDENT);
			}
		};
		outdentAction.setImageDescriptor(RichTextImages.IMG_DESC_OUTDENT);
		
		Action indentAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.INDENT);
			}
		};
		indentAction.setImageDescriptor(RichTextImages.IMG_DESC_INDENT);
		
		Action addLineAction = new Action() {
			public void run() {
				richText.executeCommand(RichTextCommand.ADD_LINE);
			}
		};
		addLineAction.setImageDescriptor(RichTextImages.IMG_DESC_ADD_LINE);		
		
		Action addLinkAction = new Action() {
			public void run() {
				AddLinkDialog dialog = new AddLinkDialog(Display.getCurrent().getActiveShell(), null);
				dialog.open();
				Link link = dialog.getLink();
				if (link.getURL().length() > 0) {
					richText.executeCommand(RichTextCommand.ADD_LINK, link.getURL());
				}
			}
		};
		addLinkAction.setImageDescriptor(RichTextImages.IMG_DESC_ADD_LINK);
		
		Action addImageAction = new Action() {
			public void run() {
				AddImageDialog dialog = new AddImageDialog(Display.getCurrent().getActiveShell());
				dialog.open();
				Image image = dialog.getImage();
				if (image.getURL().length() > 0) {
					richText.executeCommand(RichTextCommand.ADD_IMAGE, image.getURL());
				}
			}
		};
		addImageAction.setImageDescriptor(RichTextImages.IMG_DESC_ADD_IMAGE);
		
		Action addTableAction = new Action() {
			public void run() {
				AddTableDialog dialog = new AddTableDialog(Display.getCurrent().getActiveShell());
				dialog.open();
				Table table = dialog.getTable();
				int rows = table.getRows();
				int cols = table.getColumns();
				String width = table.getWidth();
				if (rows > 0 && cols > 0) {
					richText.executeCommand(RichTextCommand.ADD_TABLE, rows + "', '" + cols + "', '" + width); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		};
		addTableAction.setImageDescriptor(RichTextImages.IMG_DESC_ADD_TABLE);
		
		IActionBars bars = getViewSite().getActionBars();
		IToolBarManager toolBarMgr = bars.getToolBarManager();
		
		toolBarMgr.add(new BlockTagContribution(richText));		
		toolBarMgr.add(new FontNameContribution(richText));
		toolBarMgr.add(new FontSizeContribution(richText));		
		
		toolBarMgr.add(boldAction);
		toolBarMgr.add(italicAction);
		toolBarMgr.add(underlineAction);
		toolBarMgr.add(new Separator());
		toolBarMgr.add(subscriptAction);		
		toolBarMgr.add(superscriptAction);
		toolBarMgr.add(new Separator());
		toolBarMgr.add(justifyLeftAction);
		toolBarMgr.add(justifyCenterAction);
		toolBarMgr.add(justifyRightAction);
		toolBarMgr.add(justifyFullAction);
		toolBarMgr.add(new Separator());		
		toolBarMgr.add(addOrderedListAction);
		toolBarMgr.add(addUnorderedListAction);
		toolBarMgr.add(new Separator());		
		toolBarMgr.add(outdentAction);
		toolBarMgr.add(indentAction);
		toolBarMgr.add(new Separator());
		toolBarMgr.add(addLineAction);		
		toolBarMgr.add(addLinkAction);
		toolBarMgr.add(addImageAction);
		toolBarMgr.add(addTableAction);
		toolBarMgr.add(new Separator());		
	}
	
	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	public void setFocus() {
		if (richText != null) {
			richText.setFocus();
		}
	}
	
}