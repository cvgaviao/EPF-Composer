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
package org.eclipse.epf.library.tester.impl.testcommands;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.importing.services.PluginImportData;
import org.eclipse.epf.importing.services.PluginImportingService;
import org.eclipse.epf.importing.services.PluginImportData.PluginInfo;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCImportPlugins extends TestCommandImpl {					
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ImportFolderName, element.getAttribute(AT_ImportFolderName));
	}	
	
	public TCExeReply execute() {
		String importFolderName = getAttribute(AT_ImportFolderName);
		
		File file = getOwner().getImportFile(importFolderName);
		if (file == null) {
			return new TCExeReplyImpl("Can not locate importFolderName: " + importFolderName, false);
		}
		
		PluginImportData data = new PluginImportData();
		data.llData.setLibName(importFolderName);
		data.llData.setParentFolder(file.getAbsolutePath());		
		final PluginImportingService importService = new PluginImportingService(data);
		importService.validate(null);
		List importPlugins = data.getPlugins();
		for (int i=0; i < importPlugins.size(); i++) {
			PluginInfo info = (PluginInfo) importPlugins.get(i);
			info.selected = true;
		}
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				try {
					importService.performImport(new NullProgressMonitor());
					MethodLibrary lib  = org.eclipse.epf.library.LibraryService.getInstance().getCurrentMethodLibrary();
					getOwner().setCurrentBaseLib(lib);
				} catch (Throwable e) {
					e.printStackTrace();
				}
			}
		});
		
		return null;
	}
}

