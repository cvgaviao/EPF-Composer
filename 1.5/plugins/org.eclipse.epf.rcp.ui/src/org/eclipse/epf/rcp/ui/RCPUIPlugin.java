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

import org.eclipse.epf.common.ui.AbstractPlugin;
import org.osgi.framework.BundleContext;

/**
 * The RCP UI plug-in class.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class RCPUIPlugin extends AbstractPlugin {

	// The shared plug-in instance.
	private static RCPUIPlugin plugin;

	/**
	 * Creates a new instance.
	 */
	public RCPUIPlugin() {
		super();
		plugin = this;
	}

	/**
	 * @see org.eclipse.epf.common.ui.AbstractPlugin#start(BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * @see org.eclipse.epf.common.ui.AbstractPlugin#stop(BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	/**
	 * Returns the shared plug-in instance.
	 */
	public static RCPUIPlugin getDefault() {
		return plugin;
	}

}
