//------------------------------------------------------------------------------
//Copyright (c) 2005, 2006 IBM Corporation and others.
//All rights reserved. This program and the accompanying materials
//are made available under the terms of the Eclipse Public License v1.0
//which accompanies this distribution, and is available at
//http://www.eclipse.org/legal/epl-v10.html
//
//Contributors:
//IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.diagram.tests;

import org.eclipse.epf.common.ui.AbstractPlugin;
import org.osgi.framework.BundleContext;

/**
* The Tests plug-in class.
* 
* @author	Shilpa Toraskar
* @since	1.2
*/
public class DiagramTestPlugin extends AbstractPlugin {

	// The plug-in ID.
	public static final String PLUGIN_ID = DiagramTestPlugin.class.getName();
	
	// The shared plug-in instance.
	private static DiagramTestPlugin plugin;
	
	/**
	 * Default constructor.
	 */
	public DiagramTestPlugin() {
		super();
		plugin = this;
	}

	/**
	 * This method is called upon plug-in activation.
	 * 
	 * @see org.eclipse.epf.uma.core.plugin.AbstractPlugin#start(BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * This method is called when the plug-in is stopped.
	 * 
	 * @see org.eclipse.epf.uma.core.plugin.AbstractPlugin#start(BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	/**
	 * Returns the shared plug-in instance.
	 */
	public static DiagramTestPlugin getDefault() {
		return plugin;
	}

}
