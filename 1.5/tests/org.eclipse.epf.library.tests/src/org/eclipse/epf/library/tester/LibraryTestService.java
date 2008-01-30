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
package org.eclipse.epf.library.tester;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.common.utils.FileUtil;
import org.eclipse.epf.library.project.MethodLibraryProject;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;

/**
 * Utility class used in JUnit tests for library export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class LibraryTestService {	
	
	private static LibraryTestService instance = new LibraryTestService();	
		
	private LibraryTestService() {
	}
	
	public static LibraryTestService getInstance() {
		return instance;
	}	
			
	public MethodLibrary createLibrary(String path, String libName) {
		try {
			Map params = new HashMap();
			params.put("library.path", path); //$NON-NLS-1$
			MethodLibrary lib = org.eclipse.epf.library.LibraryService.getInstance()
					.createMethodLibrary(libName, "xmi", params); //$NON-NLS-1$ //$NON-NLS-2$
			org.eclipse.epf.library.LibraryService.getInstance().setCurrentMethodLibrary(lib);
			return lib;
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}
	
	/**
	 * close the temp library and delete the library resource files
	 *
	 */
	public void closeLibrary(MethodLibrary lib) {
		if (lib == null) {
			return;
		}
		try {
			if (lib != org.eclipse.epf.library.LibraryService.getInstance().getCurrentMethodLibrary()) {
				throw new UnsupportedOperationException();
			}
			
			File libFolder = new File(lib.eResource().getURI().toFileString()).getParentFile();
			
			org.eclipse.epf.library.LibraryService.getInstance()
				.closeCurrentMethodLibrary();		
			
			MethodLibraryProject.closeProject(libFolder.getAbsolutePath(), null);
			
		} catch(Exception ex) {
			ex.printStackTrace();
		}
	}
	
	public MethodLibrary loadLibrary(final String path) {
		final MethodLibrary[] lib = new MethodLibrary[1];
		lib[0] = null;
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				try {
					MethodLibraryProject.openProject(path, new NullProgressMonitor());
					lib[0] = LibraryUtil.loadLibrary(path + File.separator + "library.xmi");
					MethodLibraryProject.closeProject(path, new NullProgressMonitor());
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});

		return lib[0];
	}		
	
	public void deleteFolder(File folder) {
		if (folder == null) {
			return;
		}
		FileUtil.deleteAllFiles(folder.getAbsolutePath());
		folder.delete();
	}			
	
	public static MethodElement getElement(List elements, String name) {
		int sz = elements == null ? 0 : elements.size();
		if (sz == 0) {
			return null;
		}
		for (int i=0; i<sz; i++) {
			Object obj = elements.get(i);
			MethodElement elem = obj instanceof MethodElement ? (MethodElement) obj : null;
			if (elem != null && elem.getName().equals(name)) {
				return elem;
			}
		}
		return null;
	}
	
}