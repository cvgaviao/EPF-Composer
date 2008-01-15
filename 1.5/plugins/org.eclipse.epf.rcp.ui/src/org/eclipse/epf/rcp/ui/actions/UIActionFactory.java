//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/eplv10.html
//
// Contributors:
// IBM Corporation  initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.rcp.ui.actions;

import org.eclipse.epf.rcp.ui.RCPUIResources;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.RetargetAction;

/**
 * Creates actions that are specific to the EPF Composer workbench.
 * 
 * @author Bingxue Xu
 * @author Kelvin Low
 * @since 1.0
 */
public abstract class UIActionFactory extends ActionFactory {

	/**
	 * Creates a new instance.
	 */
	protected UIActionFactory(String actionId) {
		super(actionId);
	}

	/**
	 * Create a customized SHOW_INTRO action
	 */
	public static final ActionFactory SHOW_INTRO = new ActionFactory("intro") { //$NON-NLS-1$
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			IWorkbenchAction action = new IntroductionAction(window);
			action.setId(getId());
			return action;
		}
	};

	/**
	 * Create a customized OPEN_ELEMENT action that is equavilent to the EDIT menu
	 */
	public static final ActionFactory OPEN_ELEMENT = new ActionFactory("edit") { //$NON-NLS-1$
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction("OpenElement",  //$NON-NLS-1$
					RCPUIResources.editMenuItem_text);
			window.getPartService().addPartListener(action);
			action
					.setActionDefinitionId("org.eclipse.epf.rcp.ui.actions.openElement");  //$NON-NLS-1$
			ISharedImages sharedImages = window.getWorkbench()
					.getSharedImages();
			action.setImageDescriptor(sharedImages
					.getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
			action.setDisabledImageDescriptor(sharedImages
					.getImageDescriptor(ISharedImages.IMG_TOOL_COPY_DISABLED));

			return action;
		}
	};
	
	/**
	 * Create a customized IMPORT action
	 */
	public static final ActionFactory UI_IMPORT = new ActionFactory("import") { //$NON-NLS-1$
				public IWorkbenchAction create(IWorkbenchWindow window) {
					if (window == null) {
						throw new IllegalArgumentException();
					}
					IWorkbenchAction action = new UIImportResourcesAction(window);
					action.setId(getId());
					return action;
				}
	};
		
	/**
	 * Workbench action (id "export"): Opens the export wizard. This action
	 * maintains its enablement state.
	 */
	public static final ActionFactory UI_EXPORT = new ActionFactory("export") { //$NON-NLS-1$
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			IWorkbenchAction action = new UIExportResourcesAction(window);
			action.setId(getId());
			return action;
		}
	};

}
