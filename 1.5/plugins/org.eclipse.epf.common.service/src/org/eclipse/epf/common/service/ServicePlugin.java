package org.eclipse.epf.common.service;

import org.eclipse.epf.common.AbstractActivator;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class ServicePlugin extends AbstractActivator {

	// The shared plug-in instance.
	private static ServicePlugin plugin;

	/**
	 * Creates a new instance.
	 */
	public ServicePlugin() {
		super();
		plugin = this;
	}

	/**
	 * @see org.eclipse.epf.common.plugin.AbstractPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * @see org.eclipse.epf.common.plugin.AbstractPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	/**
	 * Gets the shared instance.
	 * 
	 * @return the shared plug-in instance
	 */
	public static ServicePlugin getDefault() {
		return plugin;
	}

}
