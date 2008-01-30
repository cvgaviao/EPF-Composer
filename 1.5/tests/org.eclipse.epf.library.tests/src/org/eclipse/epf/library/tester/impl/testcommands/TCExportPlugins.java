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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.export.services.PluginExportData;
import org.eclipse.epf.export.services.PluginExportService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.uma.MethodPlugin;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCExportPlugins extends TestCommandImpl {					
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ExportFolderName, element.getAttribute(AT_ExportFolderName));
	}	
	
	public TCExeReply execute() {
		List selectedPlugins = new ArrayList(getOwner().getCurrentBaseLib().getMethodPlugins());	
		
		int sz = selectedPlugins == null ? 0 : selectedPlugins.size();
		if (sz == 0) {
			return null; 
		}
		
		String exportFolderName = getAttribute(AT_ExportFolderName);
		MethodPlugin plugin = (MethodPlugin) selectedPlugins.get(0);
		File file = new File(plugin.eResource().getURI().toFileString());
		file = file.getParentFile().getParentFile();
		File exportFolder = getOwner().registerExportDestination(exportFolderName, false);
		
		final PluginExportData data = new PluginExportData();
		data.llData.setLibName(file.getName());
		data.llData.setParentFolder(exportFolder.getAbsolutePath());
		data.setSelectedPlugins(selectedPlugins);
		data.buildAssociatedConfigMap();

		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				(new PluginExportService(data)).run(new NullProgressMonitor());
			}
		});
		
		return null;
	}
}


