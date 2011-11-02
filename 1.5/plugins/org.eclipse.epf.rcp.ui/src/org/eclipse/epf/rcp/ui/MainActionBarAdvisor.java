//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.rcp.ui;

import org.eclipse.epf.authoring.ui.AuthoringUIHelpContexts;
import org.eclipse.epf.authoring.ui.views.ViewHelper;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.rcp.ui.actions.NewWizardMenu;
import org.eclipse.epf.rcp.ui.actions.UIActionFactory;
import org.eclipse.epf.rcp.ui.actions.UIOpenPerspectiveDialogAction;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ContributionItemFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.ide.IIDEActionConstants;
import org.eclipse.ui.internal.provisional.application.IActionBarConfigurer2;

/**
 * Creates, adds and disposes the actions added to a workbench window.
 * 
 * @author Kelvin Low
 * @author Bingxue Xu
 * @author Phong Nguyen Le
 * @author Jinhua Xi
 * @since 1.0
 */
public class MainActionBarAdvisor extends ActionBarAdvisor {

	// All actions should be created in makeActions and referenced in the
	// fill methods. This ensures that the actions aren't recreated
	// when fillActionBars is called with FILL_PROXY.

	// File actions.
	protected IWorkbenchAction newAction;

	protected IWorkbenchAction closeAction;

	protected IWorkbenchAction closeAllAction;

	protected IWorkbenchAction saveAction;

	protected IWorkbenchAction saveAsAction;

	protected IWorkbenchAction saveAllAction;

	protected IWorkbenchAction revertAction;

	protected IWorkbenchAction moveAction;

	protected IWorkbenchAction renameAction;

	protected IWorkbenchAction refreshAction;

	protected IWorkbenchAction importAction;

	protected IWorkbenchAction exportAction;

	protected IWorkbenchAction propertiesAction;

	protected IWorkbenchAction exitAction;

	// Edit actions.
	protected IWorkbenchAction openElementAction;

	protected IWorkbenchAction undoAction;

	protected IWorkbenchAction redoAction;

	protected IWorkbenchAction cutAction;

	protected IWorkbenchAction copyAction;

	protected IWorkbenchAction pasteAction;

	protected IWorkbenchAction deleteAction;

	protected IWorkbenchAction selectAllAction;

	protected IWorkbenchAction findAction;

	// Navigate actions.
	protected IWorkbenchAction goIntoAction;

	protected IWorkbenchAction nextAction;

	protected IWorkbenchAction previousAction;

	protected IWorkbenchAction backAction;

	protected IWorkbenchAction forwardAction;

	// Window actions.
	protected IWorkbenchAction openNewWindowAction;

	protected IWorkbenchAction openPerspectiveDialogAction;

	protected IWorkbenchAction customizePerspectiveAction;

	protected IWorkbenchAction savePerspectiveAction;

	protected IWorkbenchAction resetPerspectiveAction;

	protected IWorkbenchAction closePerspectiveAction;

	protected IWorkbenchAction closeAllPerspectiveAction;

	protected IWorkbenchAction preferencesAction;

	// Window navigation actions.
	protected IWorkbenchAction nextEditorAction;

	protected IWorkbenchAction previousEditorAction;

	protected IWorkbenchAction nextPartAction;

	protected IWorkbenchAction previousPartAction;

	protected IWorkbenchAction nextPerspectiveAction;

	protected IWorkbenchAction previousPerspectiveAction;

	// Help actions.
	protected IWorkbenchAction welcomeAction;

	protected IWorkbenchAction helpContentsAction;

	protected IWorkbenchAction aboutAction;

	protected IWorkbenchAction newWizardDropDownAction;

	protected IWorkbenchAction lockToolBarAction;

	protected IWorkbenchAction editActionSetAction;

	protected MenuManager mainHelpMenu;

	protected MenuManager mainFileMenu;

	protected MenuManager internalMenu;

	protected IWorkbenchWindow window;

	/**
	 * Creates a new instance.
	 * 
	 * @param configurer
	 *            the action bar configurer
	 */
	public MainActionBarAdvisor(IActionBarConfigurer configurer) {
		super(configurer);
		window = configurer.getWindowConfigurer().getWindow();
	}

	/**
	 * Getd the workbench window.
	 * 
	 * @return the workbench window
	 */
	public IWorkbenchWindow getWindow() {
		return window;
	}

	/**
	 * @see org.eclipse.ui.application.ActionBarAdvisor#makeActions(final
	 *      IWorkbenchWindow window)
	 */
	protected void makeActions(final IWorkbenchWindow window) {
		if (PlatformUI.getWorkbench().isClosing())
			return;

		// Creates the actions and registers them. Registering is needed to
		// ensure that key bindings work. The corresponding commands key
		// bindings are defined in the plugin.xml file. Registering also
		// provides automatic disposal of the actions when the window is closed.

		newAction = ActionFactory.NEW.create(window);
		register(newAction);

		closeAction = ActionFactory.CLOSE.create(window);
		register(closeAction);

		closeAllAction = ActionFactory.CLOSE_ALL.create(window);
		register(closeAllAction);

		saveAction = ActionFactory.SAVE.create(window);
		register(saveAction);

		saveAsAction = ActionFactory.SAVE_AS.create(window);
		register(saveAsAction);

		saveAllAction = ActionFactory.SAVE_ALL.create(window);
		register(saveAllAction);

		revertAction = ActionFactory.REVERT.create(window);
		register(revertAction);

		moveAction = ActionFactory.MOVE.create(window);
		register(moveAction);

		renameAction = ActionFactory.RENAME.create(window);
		register(renameAction);

		refreshAction = ActionFactory.REFRESH.create(window);
		register(refreshAction);

		importAction = UIActionFactory.UI_IMPORT.create(window);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(importAction,
				AuthoringUIHelpContexts.FILE_IMPORT_CONTEXT);
		register(importAction);

		exportAction = UIActionFactory.UI_EXPORT.create(window);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(exportAction,
				AuthoringUIHelpContexts.FILE_EXPORT_CONTEXT);
		register(exportAction);

		propertiesAction = ActionFactory.PROPERTIES.create(window);
		register(propertiesAction);

		exitAction = ActionFactory.QUIT.create(window);
		register(exitAction);

		openElementAction = UIActionFactory.OPEN_ELEMENT.create(window);
		register(openElementAction);

		undoAction = ActionFactory.UNDO.create(window);
		register(undoAction);

		redoAction = ActionFactory.REDO.create(window);
		register(redoAction);

		cutAction = ActionFactory.CUT.create(window);
		register(cutAction);

		copyAction = ActionFactory.COPY.create(window);
		register(copyAction);

		pasteAction = ActionFactory.PASTE.create(window);
		register(pasteAction);

		deleteAction = ActionFactory.DELETE.create(window);
		register(deleteAction);

		selectAllAction = ActionFactory.SELECT_ALL.create(window);
		register(selectAllAction);

		findAction = ActionFactory.FIND.create(window);
		register(findAction);

		goIntoAction = ActionFactory.GO_INTO.create(window);
		register(goIntoAction);

		nextAction = ActionFactory.NEXT.create(window);
		register(nextAction);

		previousAction = ActionFactory.PREVIOUS.create(window);
		register(previousAction);

		backAction = ActionFactory.BACK.create(window);
		register(backAction);

		forwardAction = ActionFactory.FORWARD.create(window);
		register(forwardAction);

		openNewWindowAction = ActionFactory.OPEN_NEW_WINDOW.create(window);
		register(openNewWindowAction);

		nextEditorAction = ActionFactory.NEXT_EDITOR.create(window);
		register(nextEditorAction);

		previousEditorAction = ActionFactory.PREVIOUS_EDITOR.create(window);
		register(previousEditorAction);

		nextPartAction = ActionFactory.NEXT_PART.create(window);
		register(nextPartAction);

		previousPartAction = ActionFactory.PREVIOUS_PART.create(window);
		register(previousPartAction);

		nextPerspectiveAction = ActionFactory.NEXT_PERSPECTIVE.create(window);
		register(nextPerspectiveAction);

		previousPerspectiveAction = ActionFactory.PREVIOUS_PERSPECTIVE
				.create(window);
		register(previousPerspectiveAction);

		openPerspectiveDialogAction = createTNGOpenPerspectiveDialogAction(window);
		openPerspectiveDialogAction.setText(RCPUIResources.otherMenuItem_text);
		register(openPerspectiveDialogAction);

		customizePerspectiveAction = ActionFactory.EDIT_ACTION_SETS
				.create(window);
		register(customizePerspectiveAction);

		savePerspectiveAction = ActionFactory.SAVE_PERSPECTIVE.create(window);
		register(savePerspectiveAction);

		resetPerspectiveAction = ActionFactory.RESET_PERSPECTIVE.create(window);
		register(resetPerspectiveAction);

		closePerspectiveAction = ActionFactory.CLOSE_PERSPECTIVE.create(window);
		register(closePerspectiveAction);

		closeAllPerspectiveAction = ActionFactory.CLOSE_ALL_PERSPECTIVES
				.create(window);
		register(closeAllPerspectiveAction);

		preferencesAction = ActionFactory.PREFERENCES.create(window);
		register(preferencesAction);

		welcomeAction = UIActionFactory.SHOW_INTRO.create(window);
		register(welcomeAction);
		ImageDescriptor imgDes = RCPUIPlugin.getDefault().getImageDescriptor(
				"full/obj16/product.gif"); //$NON-NLS-1$
		welcomeAction.setImageDescriptor(imgDes);

		helpContentsAction = ActionFactory.HELP_CONTENTS.create(window);
		register(helpContentsAction);

		aboutAction = ActionFactory.ABOUT.create(window);
		register(aboutAction);
		
		// FIXME! Filter out Project... from the drop-down menu.
		newWizardDropDownAction = IDEActionFactory.NEW_WIZARD_DROP_DOWN
				.create(window);
		register(newWizardDropDownAction);

		lockToolBarAction = ActionFactory.LOCK_TOOL_BAR.create(window);
		register(lockToolBarAction);

		editActionSetAction = ActionFactory.EDIT_ACTION_SETS.create(window);
		register(editActionSetAction);
	}

	/**
	 * @see org.eclipse.ui.application.ActionBarAdvisor#fillMenuBar(IMenuManager
	 *      menuBar)
	 */
	protected void fillMenuBar(IMenuManager menuBar) {
		if (PlatformUI.getWorkbench().isClosing())
			return;
		mainFileMenu = createFileMenu(menuBar);
		menuBar.add(mainFileMenu);
		menuBar.add(createEditMenu(menuBar));

		IMenuManager navManager = createNavigateMenu(menuBar);
		menuBar.add(navManager);
		navManager.setVisible(false);

		menuBar.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));
		menuBar.add(createWindowMenu(menuBar));

		mainHelpMenu = createHelpMenu(menuBar);
		menuBar.add(mainHelpMenu);
	}

	/**
	 * Gets the File menu manager.
	 */
	public MenuManager getFileMenuManager() {
		return mainFileMenu;
	}

	/**
	 * Gets the Help menu manager.
	 */
	public MenuManager getHelpMenuManager() {
		return mainHelpMenu;
	}

	/**
	 * @see org.eclipse.ui.application.ActionBarAdvisor#fillCoolBar(ICoolBarManager
	 *      coolBar)
	 */
	protected void fillCoolBar(ICoolBarManager coolBar) {
		if (PlatformUI.getWorkbench().isClosing())
			return;
		coolBar.add(createFileToolbar(coolBar));
	}

	/**
	 * Creates the File menu.
	 */
	protected MenuManager createFileMenu(IMenuManager menuBar) {
		MenuManager fileMenu = new MenuManager(
				RCPUIResources.fileMenuItem_text,
				IWorkbenchActionConstants.M_FILE);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.FILE_START));

		// Create the New sub menu, using the same id for it as the New action.
		String newText = RCPUIResources.fileNewMenuItem_text;
		String newId = ActionFactory.NEW.getId();
		MenuManager newMenu = new MenuManager(newText, newId) {
			public String getMenuText() {
				return super.getMenuText();
			}
		};
		newMenu.add(new Separator(newId));
		newMenu.add(new NewWizardMenu(getWindow(), null));
		newMenu.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		fileMenu.add(newMenu);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.NEW_EXT));
		
		//Alex: The below comment out code has nothing with UI, but adding confuse.
		//The real open menu come from /org.eclipse.epf.library.ui/plugin.xml
//		
//		MenuManager openSubMenu = new MenuManager(
//				RCPUIResources.fileOpenMenuItem_text,
//				IWorkbenchActionConstants.OPEN_EXT);
//		openSubMenu
//				.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));
//		openSubMenu.add(new Separator());
//		fileMenu.add(openSubMenu);
//		

		fileMenu.add(new Separator());
		fileMenu.add(closeAction);
		fileMenu.add(closeAllAction);
		fileMenu.add(new Separator());
		fileMenu.add(saveAction);
		fileMenu.add(saveAsAction);
		fileMenu.add(saveAllAction);
		fileMenu.add(revertAction);
		fileMenu.add(new Separator());
		fileMenu.add(moveAction);
		fileMenu.add(renameAction);
		fileMenu.add(refreshAction);
		fileMenu.add(new Separator());
		fileMenu.add(new GroupMarker("udt")); //$NON-NLS-1$
		fileMenu.add(new Separator());
		fileMenu.add(new GroupMarker("report")); //$NON-NLS-1$
		fileMenu.add(new Separator());
		fileMenu.add(importAction);
		fileMenu.add(exportAction);
		fileMenu.add(new Separator());
		fileMenu.add(propertiesAction);
		fileMenu.add(new Separator());
		fileMenu.add(exitAction);
		fileMenu.add(new GroupMarker(IWorkbenchActionConstants.FILE_END));

		return fileMenu;
	}

	/**
	 * Creates the Edit menu.
	 */
	protected MenuManager createEditMenu(IMenuManager menuBar) {
		MenuManager editMenu = new MenuManager(
				RCPUIResources.editMenuItem_text,
				IWorkbenchActionConstants.M_EDIT);
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.EDIT_START));
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));
		editMenu.add(new Separator());
		editMenu.add(new GroupMarker("opengroup")); //$NON-NLS-1$
		editMenu.add(openElementAction);
		editMenu.add(cutAction);
		editMenu.add(copyAction);
		editMenu.add(pasteAction);
		editMenu.add(new Separator());
		editMenu.add(deleteAction);
		editMenu.add(selectAllAction);
		editMenu.add(new Separator());
		editMenu.add(findAction);
		editMenu.add(new GroupMarker(IWorkbenchActionConstants.EDIT_END));
		return editMenu;
	}

	/**
	 * Creates the Navigate menu.
	 */
	protected MenuManager createNavigateMenu(IMenuManager menuBar) {
		MenuManager navMenu = new MenuManager(
				RCPUIResources.navigateMenuItem_text,
				IWorkbenchActionConstants.M_NAVIGATE);
		navMenu.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));
		navMenu.add(goIntoAction);
		navMenu.add(new Separator());
		navMenu.add(nextAction);
		navMenu.add(previousAction);
		navMenu.add(new Separator());
		navMenu.add(backAction);
		navMenu.add(forwardAction);
		navMenu.add(new GroupMarker(IWorkbenchActionConstants.WB_END));
		return navMenu;
	}

	/**
	 * Creates the Window menu.
	 */
	protected MenuManager createWindowMenu(IMenuManager menuBar) {
		MenuManager windowMenu = new MenuManager(
				RCPUIResources.windowMenuItem_text,
				IWorkbenchActionConstants.M_WINDOW);
		windowMenu.add(new GroupMarker(IWorkbenchActionConstants.WB_START));
		windowMenu.add(new GroupMarker(IWorkbenchActionConstants.MB_ADDITIONS));

		MenuManager changePerspMenuMgr = new MenuManager(
				RCPUIResources.windowOpenPerspectiveMenuItem_text,
				"openPerspective"); //$NON-NLS-1$
		IContributionItem changePerspMenuItem = ContributionItemFactory.PERSPECTIVES_SHORTLIST
				.create(getWindow());
		changePerspMenuMgr.add(changePerspMenuItem);
		windowMenu.add(changePerspMenuMgr);
		MenuManager showViewMenuMgr = new MenuManager(
				RCPUIResources.windowShowViewMenuItem_text, "showView"); //$NON-NLS-1$
		IContributionItem showViewMenu = ContributionItemFactory.VIEWS_SHORTLIST
				.create(getWindow());
		showViewMenuMgr.add(showViewMenu);
		windowMenu.add(showViewMenuMgr);

		windowMenu.add(new Separator());
		windowMenu.add(customizePerspectiveAction);
		windowMenu.add(savePerspectiveAction);
		windowMenu.add(resetPerspectiveAction);
		windowMenu.add(closePerspectiveAction);
		windowMenu.add(closeAllPerspectiveAction);
		windowMenu.add(new Separator());
		windowMenu.add(preferencesAction);
		windowMenu.add(new GroupMarker(IWorkbenchActionConstants.WB_END));
		return windowMenu;
	}

	protected void createInternalMenu(final IMenuManager helpMmenu) {
		internalMenu = new MenuManager(
				RCPUIResources.mainActionBarAdvisor_Diagnosis, ""); //$NON-NLS-1$

		internalMenu.add(new Action(
				RCPUIResources.mainActionBarAdvisor_HealthCheck) {
			public void run() {
				ViewHelper.checkLibraryHealth(LibraryService.getInstance().getCurrentMethodLibrary());
			}
		});
	}

	/**
	 * Creates the Help menu.
	 */
	protected MenuManager createHelpMenu(IMenuManager menuBar) {
		MenuManager helpMmenu = new MenuManager(
				RCPUIResources.helpMenuItem_text,
				IWorkbenchActionConstants.M_HELP);
		helpMmenu.add(new GroupMarker(IWorkbenchActionConstants.HELP_START));
		helpMmenu.add(welcomeAction);

		helpMmenu.add(new Separator());
		helpMmenu.add(helpContentsAction);
		helpMmenu.add(new GroupMarker("group.help")); //$NON-NLS-1$

		helpMmenu.add(new Separator());
		helpMmenu.add(new GroupMarker("group.assist")); //$NON-NLS-1$

		helpMmenu.add(new Separator());
		helpMmenu.add(new GroupMarker("group.tools")); //$NON-NLS-1$

		helpMmenu.add(new Separator());
		helpMmenu.add(new GroupMarker("group.updates")); //$NON-NLS-1$

		// Menu items for internal tool
		helpMmenu.add(new Separator());
		helpMmenu.add(new GroupMarker("group.internal")); //$NON-NLS-1$
		createInternalMenu(helpMmenu);

		helpMmenu.add(new Separator());
		helpMmenu.add(aboutAction);
		helpMmenu.add(new GroupMarker(IWorkbenchActionConstants.HELP_END));

		return helpMmenu;
	}

	/**
	 * Creates the File toolbar.
	 */
	protected IToolBarManager createFileToolbar(ICoolBarManager coolBar) {
		IActionBarConfigurer2 actionBarConfigurer = (IActionBarConfigurer2) getActionBarConfigurer();
		IMenuManager popUpMenu = new MenuManager();
		popUpMenu.add(new ActionContributionItem(lockToolBarAction));
		popUpMenu.add(new ActionContributionItem(editActionSetAction));
		coolBar.setContextMenuManager(popUpMenu);

		coolBar.add(new GroupMarker(IIDEActionConstants.GROUP_FILE));
		IToolBarManager fileToolBar = actionBarConfigurer
				.createToolBarManager();
		fileToolBar.add(new Separator(IWorkbenchActionConstants.NEW_GROUP));
		fileToolBar.add(newWizardDropDownAction);
		fileToolBar.add(new GroupMarker(IWorkbenchActionConstants.NEW_EXT));
		fileToolBar.add(new GroupMarker(IWorkbenchActionConstants.SAVE_GROUP));
		fileToolBar.add(saveAction);
		fileToolBar.add(saveAllAction);
		fileToolBar.add(new GroupMarker(IWorkbenchActionConstants.SAVE_EXT));
		fileToolBar.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));

		return fileToolBar;
	}

	/**
	 * Create a custom open perspective dialog
	 * 
	 * @param window
	 * @return
	 */
	protected IWorkbenchAction createTNGOpenPerspectiveDialogAction(
			IWorkbenchWindow window) {
		if (window == null) {
			throw new IllegalArgumentException();
		}
		IWorkbenchAction action = new UIOpenPerspectiveDialogAction(window);
		action.setId("openPerspectiveDialog"); //$NON-NLS-1$
		return action;
	}

}
