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
package org.eclipse.epf.library;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.epf.common.AbstractActivator;
import org.eclipse.epf.library.layout.LayoutResources;
import org.osgi.framework.BundleContext;

/**
 * The Library plug-in class.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class LibraryPlugin extends AbstractActivator {

	private static final String LAYOUT_PATH = "layout/"; //$NON-NLS-1$;

	public static final String LAYOUT_XSL_PATH = "layout/xsl/"; //$NON-NLS-1$;
	private static final String LAYOUT_CSS_PATH = "layout/css/"; //$NON-NLS-1$;

	public static final String LAYOUT_SCRIPTS_FOLDER = "scripts"; //$NON-NLS-1$;

	public static final String LAYOUT_SCRIPTS_PATH = "layout/scripts/"; //$NON-NLS-1$;

	// The shared plug-in instance.
	private static LibraryPlugin plugin;

	private String layoutPath, layoutXslPath, layoutCssPath;

	/**
	 * Creates a new instance.
	 */
	public LibraryPlugin() {
		super();
		plugin = this;
	}

	/**
	 * @see org.eclipse.epf.common.ui.AbstractPlugin#start(BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		try {
			URL url = new URL(super.getInstallURL(), LAYOUT_PATH);
			layoutPath = FileLocator.resolve(url).getPath();

			url = new URL(super.getInstallURL(), LAYOUT_XSL_PATH);
			layoutXslPath = FileLocator.resolve(url).getPath();

			url = new URL(super.getInstallURL(), LAYOUT_CSS_PATH);
			layoutCssPath = FileLocator.resolve(url).getPath();

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * @see org.eclipse.epf.common.ui.AbstractPlugin#start(BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;

		LayoutResources.clear();
	}

	/**
	 * Returns the shared plug-in instance.
	 */
	public static LibraryPlugin getDefault() {
		return plugin;
	}

	public String getLayoutPath() {
		return layoutPath;
	}

	public String getLayoutXslPath() {
		return layoutXslPath;
	}

	public String getLayoutCssPath() {
		return layoutCssPath;
	}
}