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

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.export.services.ConfigurationExportData;
import org.eclipse.epf.export.services.ConfigurationExportService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCExportConfiguration extends TestCommandImpl {				
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ExportFolderName, element.getAttribute(AT_ExportFolderName));
		setAttribute(AT_SelectedConfigName, element.getAttribute(AT_SelectedConfigName));
	}	
	
	public TCExeReply execute() {
		TestCommandMgr owner = getOwner();
		MethodLibrary currLib = owner.getCurrentBaseLib();
		if (currLib == null) {
			return null;
		}
		
		String exportFolderName = getAttribute(AT_ExportFolderName);		
		String selectedConfigName = getAttribute(AT_SelectedConfigName);
		
		MethodConfiguration config = (MethodConfiguration) LibraryTestService.getElement
						(currLib.getPredefinedConfigurations(), selectedConfigName);
		if (config == null) {
			return null;
		}
		
		File exportFolder = owner.registerExportDestination(exportFolderName, false);				
		final ConfigurationExportData data = new ConfigurationExportData();
		data.llData.setLibName(currLib.getName());
		data.llData.setParentFolder(exportFolder.getAbsolutePath());
		data.exportOneConfig = true;
		data.exportConfigSpecs = false;
		data.selectedConfigs = new ArrayList();
		data.selectedConfigs.add(config);
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				(new ConfigurationExportService(data)).run(new NullProgressMonitor());
			}
		});
		
		return null;
	}
}
