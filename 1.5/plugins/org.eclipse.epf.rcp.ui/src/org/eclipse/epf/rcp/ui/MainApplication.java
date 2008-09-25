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

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.common.service.utils.CommandLineRunUtil;
import org.eclipse.epf.library.ui.LibraryUIManager;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

/**
 * This class controls all aspects of the application's execution.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class MainApplication implements IApplication {
	
	// Data mode constants for user, configuration and data locations.
	private static final String NONE = "@none"; //$NON-NLS-1$
	private static final String NO_DEFAULT = "@noDefault"; //$NON-NLS-1$
	private static final String USER_HOME = "@user.home"; //$NON-NLS-1$
	private static final String USER_DIR = "@user.dir"; //$NON-NLS-1$
	public static final String PROP_USER_HOME = "user.home"; //$NON-NLS-1$
	public static final String PROP_USER_DIR = "user.dir"; //$NON-NLS-1$

	/*
	 * @see org.eclipse.equinox.app.IApplication#start(org.eclipse.equinox.app.IApplicationContext)
	 */
	public Object start(IApplicationContext context) {
		Display display = PlatformUI.createDisplay();
		Shell shell = new Shell(display, SWT.ON_TOP);
		try {
			boolean noLock = false;
			String[] appArgs = Platform.getApplicationArgs();
			for (int i = 0; i < appArgs.length; i++) {
				if (appArgs[i].equalsIgnoreCase("-library") && i < (appArgs.length - 1)) { //$NON-NLS-1$
					String location = appArgs[i + 1];
					location = buildLocation(location);
					if (location != null) {
						LibraryUIManager.setCommandLineLibrary(location);
					}
				} else if (appArgs[i].equalsIgnoreCase("-defaultlibrary") && i < (appArgs.length - 1)) { //$NON-NLS-1$
					if (! CommandLineRunUtil.getInstance().isNeedToRun()) {
						String location = appArgs[i + 1];
						location = buildLocation(location);
						if (location != null) {
							LibraryUIManager.setCommandLineDefaultLibrary(location);
						}
					}
				} else if (appArgs[i].equalsIgnoreCase("-nolock")) { //$NON-NLS-1$
					noLock = true;
				} else if (appArgs[i].equalsIgnoreCase("-batch")) { //$NON-NLS-1$
					CommandLineRunUtil.getInstance().setNeedToRun(true);
				}
			}

			if (!noLock && !checkWorkspaceLock(shell)) {
				Platform.endSplash();
				return IApplication.EXIT_OK;
			}

			int returnCode = createWorkbenchAdvisor(display);
			if (returnCode == PlatformUI.RETURN_RESTART) {
				return IApplication.EXIT_RESTART;
			}
			return IApplication.EXIT_OK;
		} finally {
			if (shell != null) {
				shell.dispose();
			}
			display.dispose();
		}
	}

	/*
	 * @see org.eclipse.equinox.app.IApplication#stop()
	 */
	public void stop() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		if (workbench == null)
			return;
		final Display display = workbench.getDisplay();
		display.syncExec(new Runnable() {
			public void run() {
				if (!display.isDisposed()) {
					workbench.close();
				}
			}
		});
	}

	/**
	 * Creates the workbench advisor.
	 */
	protected int createWorkbenchAdvisor(Display display) {
		return PlatformUI.createAndRunWorkbench(display,
				new MainWorkbenchAdvisor());
	}

	/**
	 * Checks the workspace lock that is used to prevent a library been opened
	 * multiple times.
	 */
	protected boolean checkWorkspaceLock(Shell shell) {
		Location workspaceLocation = Platform.getInstanceLocation();
		String appName = Platform.getProduct().getName();
		try {
			if (workspaceLocation.lock()) {
				return true;
			}
			MessageDialog.openInformation(shell, NLS.bind(
					RCPUIResources.workspaceCannotLockTitle, appName), NLS
					.bind(RCPUIResources.workspaceCannotLockMessage, appName));
		} catch (IOException e) {
			RCPUIPlugin.getDefault().getLogger().logError(
					NLS
							.bind(RCPUIResources.workspaceCannotLockMessage,
									appName), e);
			MessageDialog.openInformation(shell, NLS.bind(
					RCPUIResources.workspaceCannotLockTitle, appName), e
					.getMessage());
		}
		return false;
	}

	/*
	 * taken from org.eclipse.core.runtime.adaptor.LocationManager.buildLocation
	 */
	private String buildLocation(String location) {
		String trimmedLocation = location.trim();
		String result = trimmedLocation;
		if (trimmedLocation.equalsIgnoreCase(NONE))
			return null;
		if (trimmedLocation.equalsIgnoreCase(NO_DEFAULT))
			return null;
		if (trimmedLocation.startsWith(USER_HOME)) {
			String base = substituteVar(location, USER_HOME, PROP_USER_HOME);
			result = new File(base).getAbsolutePath();
		} else if (trimmedLocation.startsWith(USER_DIR)) {
			String base = substituteVar(location, USER_DIR, PROP_USER_DIR);
			result = new File(base).getAbsolutePath();
		}
		return result;
	}
	
	private static String substituteVar(String source, String var, String prop) {
		String value = System.getProperty(prop, ""); //$NON-NLS-1$
		return value + source.substring(var.length());
	}

}
