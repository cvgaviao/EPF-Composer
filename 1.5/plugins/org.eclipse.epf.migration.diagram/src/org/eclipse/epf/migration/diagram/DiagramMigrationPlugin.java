package org.eclipse.epf.migration.diagram;

import org.eclipse.epf.common.ui.AbstractPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 * @author Shilpa Toraskar
 * @since 1.2
 */
public class DiagramMigrationPlugin extends AbstractPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.eclipse.epf.migration.diagram"; //$NON-NLS-1$

	// The shared instance
	private static DiagramMigrationPlugin plugin;
	
	/**
	 * The constructor
	 */
	public DiagramMigrationPlugin() {
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static DiagramMigrationPlugin getDefault() {
		return plugin;
	}

}
