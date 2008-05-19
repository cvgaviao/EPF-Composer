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

import java.net.URL;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.authoring.ui.AuthoringPerspective;
import org.eclipse.epf.common.service.utils.CommandLineRunUtil;
import org.eclipse.epf.common.serviceability.Logger;
import org.eclipse.epf.persistence.refresh.RefreshJob;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.eclipse.ui.internal.ide.model.WorkbenchAdapterBuilder;
import org.osgi.framework.Bundle;

/**
 * Creates the window advisor and specifies the perspective id for the initial
 * window.
 * 
 * @author Bingxue Xu
 * @author Kelvin Low
 * @author Phong Nguyen Le
 * @since 1.0
 */
public class MainWorkbenchAdvisor extends WorkbenchAdvisor {

	protected Logger logger;

	private void declareWorkbenchImage(Bundle ideBundle, String symbolicName,
			String path, boolean shared) {
		URL url = FileLocator.find(ideBundle, new Path(path), null);
		ImageDescriptor desc = ImageDescriptor.createFromURL(url);
		getWorkbenchConfigurer().declareImage(symbolicName, desc, shared);
	}

	/**
	 * Declares all IDE-specific workbench images that are used by some bundled
	 * plugins
	 * 
	 * @see IWorkbenchConfigurer#declareImage
	 */
	private void declareWorkbenchImages() {
		final String ICONS_PATH = "$nl$/icons/full/";//$NON-NLS-1$
		final String PATH_OBJECT = ICONS_PATH + "obj16/"; // Model object // icons//$NON-NLS-1$
		final String PATH_ETOOL = ICONS_PATH + "etool16/"; // Enabled toolbar icons.//$NON-NLS-1$

		Bundle ideBundle = Platform.getBundle("org.eclipse.ui.ide"); //$NON-NLS-1$

		declareWorkbenchImage(ideBundle, IDE.SharedImages.IMG_OBJ_PROJECT,
				PATH_OBJECT + "prj_obj.gif", true); //$NON-NLS-1$
		declareWorkbenchImage(ideBundle,
				IDE.SharedImages.IMG_OBJ_PROJECT_CLOSED, PATH_OBJECT
						+ "cprj_obj.gif", true); //$NON-NLS-1$

		declareWorkbenchImage(ideBundle,
				IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH, PATH_OBJECT
						+ "error_tsk.gif", true); //$NON-NLS-1$
		declareWorkbenchImage(ideBundle,
				IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH, PATH_OBJECT
						+ "warn_tsk.gif", true); //$NON-NLS-1$
		declareWorkbenchImage(ideBundle,
				IDEInternalWorkbenchImages.IMG_OBJS_INFO_PATH, PATH_OBJECT
						+ "info_tsk.gif", true); //$NON-NLS-1$  	  	
		declareWorkbenchImage(ideBundle,
				IDEInternalWorkbenchImages.IMG_ETOOL_PROBLEM_CATEGORY,
				PATH_ETOOL + "problem_category.gif", true); //$NON-NLS-1$
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#initialize(IWorkbenchConfigurer)
	 */
	public void initialize(IWorkbenchConfigurer configurer) {
		logger = RCPUIPlugin.getDefault().getLogger();
		super.initialize(configurer);
		configurer.setSaveAndRestore(true);

		// Remember the install dir and handle the -library command line
		// parameter.
		String appStartDir = System.getProperty("user.dir"); //$NON-NLS-1$
		logger.logInfo(RCPUIResources.startup_dir_log_info_text + appStartDir); 

		declareWorkbenchImages();
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#createWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer)
	 */
	public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(
			IWorkbenchWindowConfigurer configurer) {
		return new MainWorkbenchWindowAdvisor(configurer);
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId()
	 */
	public String getInitialWindowPerspectiveId() {
		if (PlatformUI.getWorkbench().isClosing())
			return null;
		return AuthoringPerspective.PERSPECTIVE_ID;
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#preStartup()
	 */
	public void preStartup() {
		super.preStartup();
		WorkbenchAdapterBuilder.registerAdapters();
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#preShutdown()
	 */
	public boolean preShutdown() {
		// Remember whether the welcome view exists or not before exiting.
		IWorkbenchWindow activeWindow = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow();
		if (activeWindow != null) {

			IWorkbenchPage page = activeWindow.getActivePage();
			if (page != null) {
				if (page instanceof WorkbenchPage) {
					IViewReference[] vwRefList = page.getViewReferences();

					boolean introViewExists = false;
					for (int i = 0; i < vwRefList.length; i++) {
						if (vwRefList[i].getId().equals(
								"org.eclipse.ui.internal.introview")) { //$NON-NLS-1$
							introViewExists = true;
							break;
						}
					}

					IPreferenceStore store = RCPUIPlugin.getDefault()
							.getPreferenceStore();
					store
							.setValue(
									"welcome_intro_view_exists", introViewExists); //$NON-NLS-1$
					RCPUIPlugin.getDefault().savePluginPreferences();
				}
			}
		}
		return true;
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#postShutdown()
	 */
	public void postShutdown() {
		try {
			// Save the workspace and remove .snap file in the
			// workspace\.metadata\.plugins\org.eclipse.core.resources folder.
			ResourcesPlugin.getWorkspace().save(true, null);
		} catch (CoreException e) {
		}
	}

	/*
	 * @see org.eclipse.ui.application.WorkbenchAdvisor#postStartup()
	 */
	public void postStartup() {
		CommandLineRunUtil runUtil = CommandLineRunUtil.getInstance();
		if (runUtil.isNeedToRun()) {
			runUtil.execute(Platform.getApplicationArgs());

			final IWorkbench workbench = getWorkbenchConfigurer()
					.getWorkbench();
			workbench.getDisplay().asyncExec(new Runnable() {
				public void run() {
					if (! RefreshJob.getInstance().cancel()) {
						try {
							RefreshJob.getInstance().join();
						} catch (Exception e) {							
						}
					}
					RefreshJob.getInstance().setEnabled(false);
					RefreshJob.getInstance().reset();
					workbench.close();
				}
			});
		}

		super.postStartup();
	}

}
