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
package org.eclipse.epf.authoring.ui.util;

import java.util.ArrayList;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.util.LibraryProblemMonitor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;
import org.eclipse.ui.views.markers.WorkbenchMarkerResolution;

/**
 * Generates LibraryProblemMarkerResolution's
 * @author Weiping Lu
 * @since  1.5
 */
public class LibraryProblemMarkerResolutionGenerator implements IMarkerResolutionGenerator {
	
	private static final IMarkerResolution[] EMPTY_RESOLUTIONS = new IMarkerResolution[0];
	
	private static boolean localDebug = true;

	public IMarkerResolution[] getResolutions(IMarker marker) {
		try {
			if (marker.getType() == LibraryProblemMonitor.UnresolvedBasedPluginMARKER_ID) {
				return new IMarkerResolution[] {
						new MissingPluginResolution(marker),
						};
			}
		} catch (Exception e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
		}
		return EMPTY_RESOLUTIONS;
	}
	
	
	private static class MissingPluginResolution extends WorkbenchMarkerResolution {
		private IMarker currentMarker;

		public MissingPluginResolution(IMarker currentMarker) {
			this.currentMarker = currentMarker;
		}
		
		/**
		 * @see org.eclipse.ui.IMarkerResolution#getLabel()
		 */
		public String getLabel() {
			return AuthoringUIResources.LibraryProblemMarkerResolutionGenerator_removeUnresolvedBasePlugins;
		}

		public void run(IMarker marker) {
			ILibraryManager libMgr = LibraryService.getInstance().getCurrentLibraryManager();
			if (libMgr == null) {
				return;
			}
			
			libMgr.getLibraryProblemMonitor().fixProblem(marker);
		}

		@Override
		public IMarker[] findOtherMarkers(IMarker[] markers) {
			ArrayList<IMarker> similarMarkerList = new ArrayList<IMarker>();
			for (int i = 0; i < markers.length; i++) {
				IMarker marker = markers[i];
				if(!currentMarker.equals(marker)) {
					try {
						if(LibraryProblemMonitor.UnresolvedBasedPluginMARKER_ID.equals(marker.getType())) {
							similarMarkerList.add(marker);
						}
					} catch (CoreException e) {
						AuthoringUIPlugin.getDefault().getLogger().logError(e);
					}
				}
			}
			IMarker[] similarMarkers = new IMarker[similarMarkerList.size()];
			similarMarkerList.toArray(similarMarkers);
			return similarMarkers;
		}

		public String getDescription() {
			// TODO provide detailed description
			return null;
		}

		public Image getImage() {
			// TODO provide image
			return null;
		}
		
	}
	

	
}
