//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.common.utils.RestartableJob;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.events.ILibraryChangeListener;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;

/**
 * The class for monitor library problems.
 * 
 * @author Weiping Lu
 * @since 1.5
 */

public class LibraryProblemMonitor extends RestartableJob implements ILibraryChangeListener {
	
	public static final String MARKER_ID = LibraryPlugin.getDefault().getId() + ".missingBasePlugins"; //$NON-NLS-1$
	private ILibraryManager libMgr;
	private long delay = 1000L;
	private boolean kickToRunBit = false;
	private boolean kicked = true;
	private Thread monitorThread;
	private boolean stopMonitor = false;
	private Map<MethodPlugin, IMarker> pluginMarkerMap = new HashMap<MethodPlugin, IMarker>();
	
	public LibraryProblemMonitor(ILibraryManager libMgr) {
		super("Problem Monitor");
		this.libMgr = libMgr;
		startMonitor();
	}
	
	public void startMonitor() {
		Runnable runnable = new Runnable() {
			public void run() {
				while (! stopMonitor) {
					try {
						if (kickToRunBit) {
							kickToRunBit = false;
						} else {
							Thread.sleep(delay);
						}
					} catch (Exception e) {
						LibraryPlugin.getDefault().getLogger().logError(e);
						return;
					}
					if (isKicked()) {
						setKicked(false);
						guardedSchedule(0L);
					}
				}
			}
		};
		
		stopMonitor = false;
		if (monitorThread == null) {
			monitorThread = new Thread(runnable);
		}
		monitorThread.start();
	}
	
	public void stopMonitor() {
		stopMonitor = true;
	}
	
	
	public void libraryChanged(int option, Collection<Object> changedItems) {
		if (changedItems != null && changedItems.size() > 0) {
			enableToRestart();
			setKicked(true);
		}
	}
	
	// Update will abort and re-schedule a job if new change detected
	private void update()  throws RestartInterruptException {
		if (localDebug) {
			System.out.println("LD> update");
		}
		MethodLibrary lib = libMgr.getMethodLibrary();
		if (lib == null) {
			if (localDebug) {
				System.out.println("LD> lib == null");
			}
			return;
		}
		checkRestartInterruptException(delay);		

		cleanUp();
		
		List<MethodPlugin> plugins = new ArrayList<MethodPlugin>();
		plugins.addAll(lib.getMethodPlugins());
		
		Set<MethodPlugin> pluginSet = new HashSet<MethodPlugin>(plugins);
		for (MethodPlugin plugin: plugins) {
			checkRestartInterruptException(delay);
			List<MethodPlugin> baseList = plugin.getBases();
			boolean missing = false;
			for (MethodPlugin base: baseList) {
				checkRestartInterruptException(delay);
				if (! pluginSet.contains(base)) {					
					if (! missing) {
						System.out.println("LD> plugin: " + plugin);
						System.out.println("LD> " + plugin.getName() + " is missing bases:");
						missing = true;
					}
					System.out.println("LD> base: " + base);
					addMissingBasePluginError(plugin, base);
				}
			}
			if (missing) {
				System.out.println("");
			}
		}		
		
	}
	
	private void addMissingBasePluginError(MethodPlugin plugin, MethodPlugin base) {
		IMarker marker = pluginMarkerMap.get(plugin);
		if (marker == null) {
			Resource res = plugin.eResource();
			if (res == null) {
				return;
			}
		
			URI containerURI = res.getURI();
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IPath path = new Path(containerURI.toFileString());
			IFile file = workspace.getRoot().getFileForLocation(path);
			if (file == null) {
				return;
			}
			String location = containerURI != null ? containerURI
					.toFileString() : ""; //$NON-NLS-1$	
			try {
				marker = file.createMarker(MARKER_ID);
				marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				marker.setAttribute("name", plugin.getName());
				marker.setAttribute("guid", plugin.getGuid());
				marker.setAttribute(IMarker.LOCATION, location);
				marker.setAttribute("guid", plugin.getGuid());	

			} catch (Exception e) {
				LibraryPlugin.getDefault().getLogger().logError(e);
			}
			if (marker == null) {
				return;
			}
			if (localDebug) {
				System.out.println("LD> marker: " + marker);
			}
			pluginMarkerMap.put(plugin, marker);				
		
		}
		
		String missingBaseGuids = null;
		try {

			String errMsg = plugin.getName();
			missingBaseGuids = (String) marker.getAttribute("missingBaseGuids");
			if (missingBaseGuids == null || missingBaseGuids.length() == 0) {
				missingBaseGuids = base.getGuid();
				errMsg += " misses base plugin: " + missingBaseGuids;
			} else if (missingBaseGuids.indexOf(base.getGuid()) < 0) {
				missingBaseGuids += ", base.getGuid()";
				errMsg += " misses base plugins: " + missingBaseGuids;
			}
			marker.setAttribute(IMarker.MESSAGE, errMsg);
		} catch (Exception e) {
			LibraryPlugin.getDefault().getLogger().logError(e);
		}

	}
	
	protected IStatus restartableRun(IProgressMonitor monitor) throws RestartInterruptException {
		update();
		return Status.OK_STATUS;
	}
	
	protected void resetToRestart() {
		if (localDebug) {
			System.out.println("LD> resetToRestart");
		}
		cleanUp();
	}
	
	private void cleanUp() {
		if (pluginMarkerMap.isEmpty()) {
			return;
		}
		for (IMarker marker: pluginMarkerMap.values()) {
			try {
				marker.delete();
			} catch (Exception e) {
				LibraryPlugin.getDefault().getLogger().logError(e);
			}
		}
		pluginMarkerMap = new HashMap<MethodPlugin, IMarker>();
	}

	private boolean isKicked() {
		return kicked;
	}

	public void kickToRun() {
		kickToRunBit = true;
		enableToRestart();
		setKicked(true);
	}
	
	protected void setKicked(boolean kicked) {
		this.kicked = kicked;
		if (localDebug) {
			System.out.println("LD> setKicked: " + kicked);
		}
	}
	
	public void dispose() {
		stopMonitor();
		cleanUp();
	}

}
