package org.eclipse.epf.toolbox;

import java.util.Date;

import org.eclipse.epf.common.ui.AbstractPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class ToolboxPlugin extends AbstractPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = ToolboxPlugin.class.getName();
	
	// The shared plug-in instance.
	private static ToolboxPlugin plugin;
	
	// The date and time when this plug-in was started.
	private Date startTime;
	
	/**
	 * Default constructor.
	 */
	public ToolboxPlugin() {
		super();
		plugin = this;
	}
	
	public void start(BundleContext context) throws Exception {
		super.start(context);
		startTime = new Date();		
	}
	
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}
	
	/**
	 * Returns the shared plug-in instance.
	 */
	public static ToolboxPlugin getDefault() {
		return plugin;
	}
	
	/**
	 * Returns the date and time when this plug-in was started.
	 */
	public Date getStartTime() {
		return startTime;
	}	

}
