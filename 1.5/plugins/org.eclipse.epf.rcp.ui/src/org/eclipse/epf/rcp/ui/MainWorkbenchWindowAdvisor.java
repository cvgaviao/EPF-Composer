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
package org.eclipse.epf.rcp.ui;

import org.eclipse.epf.library.ui.LibraryUIManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.internal.ActionSetContributionItem;
import org.eclipse.ui.internal.PluginActionContributionItem;
import org.eclipse.ui.intro.IIntroPart;

/**
 * The application specific workbench window advisor.
 * 
 * @author Bingxue Xu
 * @author Kelvin Low
 * @since 1.0
 * fix to hide the working set and external tool bar buttons @ 2007-06-14
 */
public class MainWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

	protected static String[] fileMenuRemovalList = {
			"org.eclipse.ui.openLocalFile", //$NON-NLS-1$
			"org.eclipse.ui.edit.text.newTextEditor", //$NON-NLS-1$
			"org.eclipse.ui.edit.text.openExternalFile", //$NON-NLS-1$
			"converstLineDelimitersTo" //$NON-NLS-1$
	};

	protected MainActionBarAdvisor mainActionBar;

	private static final String	ANNOTATIONNAVIGATION_ID		= "org.eclipse.ui.edit.text.actionSet.annotationNavigation";	//$NON-NLS-1$
	private static final String	WORKINGSETACTIONSET_ID		= "org.eclipse.ui.WorkingSetActionSet";							//$NON-NLS-1$
//	private static final String	EXTERNALTOOLSSET_ID			= "org.eclipse.ui.externaltools.ExternalToolsSet";				//$NON-NLS-1$
	
	private IPerspectiveListener perspectiveListener = new IPerspectiveListener() {
		public void perspectiveChanged(IWorkbenchPage page,
				IPerspectiveDescriptor perspective, String changeId) {
			LibraryUIManager.getInstance().checkConfigurationContribution();
			LibraryUIManager.getInstance().startupOpenLibrary();
		}
		
		public void perspectiveActivated(IWorkbenchPage page,
				IPerspectiveDescriptor perspective) {
			// do not show the WorkingSetsactionSet toolbar buttons
			page.hideActionSet(WORKINGSETACTIONSET_ID); 
			// do not show the External Tools button on the toolbar
//			page.hideActionSet(EXTERNALTOOLSSET_ID);
			page.hideActionSet(ANNOTATIONNAVIGATION_ID);
		}
	};
	
	/**
	 * Creates a new instance.
	 * 
	 * @param configurer
	 *            An object for configuring the workbench window.
	 */
	public MainWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
		super(configurer);
	}

	/**
	 * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#createActionBarAdvisor(IActionBarConfigurer)
	 */
	public ActionBarAdvisor createActionBarAdvisor(
			IActionBarConfigurer configurer) {
		mainActionBar = new MainActionBarAdvisor(configurer);
		return mainActionBar;
	}

	/**
	 * add operations to be performed at postWindowCreate
	 */
	public void postWindowCreate() {
		IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
		Object data = configurer.getData("isRestoredWorkbench"); //$NON-NLS-1$
		if (data == null) {
			configurer.getWindow().getShell().setLocation(30, 30);
			configurer.getWindow().getShell().setSize(1024, 760);
		}
	}

	/**
	 * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#preWindowOpen()
	 */
	public void preWindowOpen() {
		IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
		configurer.setShowCoolBar(true);
		configurer.setShowPerspectiveBar(true);
		configurer.setShowStatusLine(true);
		configurer.setShowProgressIndicator(true);
		configurer.setTitle(RCPUIPlugin.getDefault().getString("productName")); //$NON-NLS-1$
	}

	/**
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#postWindowOpen()
	 */
	public void postWindowOpen() {
		if (PlatformUI.getWorkbench().isClosing())     
			return;                                    
		IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
		configurer.getWindow().getShell().setImage(
				RCPUIPlugin.getDefault().getSharedImage("full/obj16/product.gif")); //$NON-NLS-1$

		MenuManager fileMmenu = mainActionBar.getFileMenuManager();
		IContributionItem[] items = fileMmenu.getItems();
		for (int i = 0; i < items.length; i++) {
			if (needToRemoveFrom(items[i].getId(), fileMenuRemovalList))
				fileMmenu.remove(items[i]);
		}

		// this is a hack to change the Help | Software Updates | Manage
		// Configurations to Manage Software Configuration...
		MenuManager helpMmenu = mainActionBar.getHelpMenuManager();
		items = helpMmenu.getItems();
		for (int i = 0; i < items.length; i++) {
			if (items[i] instanceof ActionSetContributionItem) {
				ActionSetContributionItem element = (ActionSetContributionItem) items[i];
				if (element.getId().equals("org.eclipse.update.ui.updateMenu")) { //$NON-NLS-1$
					IContributionItem contribMenuMgr = element.getInnerItem();
					IContributionItem[] subMenuItems = ((MenuManager) contribMenuMgr)
							.getItems();
					for (int j = 0; j < subMenuItems.length; j++) {
						IContributionItem innerItem = ((ActionSetContributionItem) subMenuItems[j])
								.getInnerItem();
						if (innerItem instanceof PluginActionContributionItem
								&& innerItem.getId().equals(
										"org.eclipse.update.ui.configManager")) { //$NON-NLS-1$
							IAction action = ((PluginActionContributionItem) innerItem)
									.getAction();
							action
									.setText(RCPUIResources.menu_help_software_updates_manage_software_config_text); 
						}
					}
				}
			}
		}
		
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().addPerspectiveListener(perspectiveListener);
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		// do not show the WorkingSetsactionSet toolbar buttons
		page.hideActionSet(WORKINGSETACTIONSET_ID); 
		// do not show the External Tools button on the toolbar
//		page.hideActionSet(EXTERNALTOOLSSET_ID);
		page.hideActionSet(ANNOTATIONNAVIGATION_ID);	
		
	}

	private boolean needToRemoveFrom(String id, String[] list) {
		boolean rc = false;

		for (int i = 0; i < list.length; i++) {
			if (list[i].equals(id)) {
				rc = true;
				break;
			}
		}
		return rc;
	}

	/**
	 * add custom operations at postWindowRestore
	 */
	public void postWindowRestore() throws WorkbenchException {
		IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
		configurer.setData("isRestoredWorkbench", "true"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	/**
	* Customize openIntro to avoid an Eclipse bug
	* Customizes it so it will open in the standby mode if this is a restored
	* session and the last session had the IntroView open, otherwise close it
	* 
	*/
	public void openIntro() {
		super.openIntro();

		IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
		Object data = configurer.getData("isRestoredWorkbench"); //$NON-NLS-1$
		if (data == null) {
			// if not restored workspace, just exec the default in super.
			return;
		}

		// always allows the intro view to open first, and then put it into
		// standby mode
		// otherwise other fast views such as search help will be open-ed as
		// full mode
		// even the welcome page is closed.
		// if (isIntroViewExistsInLastSession()) {
		IWorkbenchConfigurer wbConfig = getWindowConfigurer()
				.getWorkbenchConfigurer();
		IIntroPart introPart = wbConfig.getWorkbench().getIntroManager()
				.getIntro();
		wbConfig.getWorkbench().getIntroManager().setIntroStandby(introPart,
				true);
		// } else {
		if (!isIntroViewExistsInLastSession()) {
			closeIntroView();
		}
	}

	private boolean isIntroViewExistsInLastSession() {
		IPreferenceStore store = RCPUIPlugin.getDefault().getPreferenceStore();
		return store.getBoolean("welcome_intro_view_exists"); //$NON-NLS-1$
	}

	private void closeIntroView() {
		try {
			IViewPart vp = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
					.getActivePage().findView(
							"org.eclipse.ui.internal.introview"); //$NON-NLS-1$
			PlatformUI.getWorkbench().getActiveWorkbenchWindow()
					.getActivePage().hideView(vp);

		} catch (Exception e) {

		}
	}

}
