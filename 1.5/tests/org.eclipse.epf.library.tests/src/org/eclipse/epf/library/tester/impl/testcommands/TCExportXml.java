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

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.export.xml.services.ExportXMLData;
import org.eclipse.epf.export.xml.services.ExportXMLService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCExportXml extends TestCommandImpl {				
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ExportFolderName, element.getAttribute(AT_ExportFolderName));
	}	
	
	public TCExeReply execute() {
		TestCommandMgr owner = getOwner();
		String exportFolderName = getAttribute(AT_ExportFolderName);
		
		MethodLibrary lib = owner.getCurrentBaseLib();
		File file = new File(lib.eResource().getURI().toFileString());
		file = file.getParentFile().getParentFile();
		File exportFile = owner.registerExportDestination(exportFolderName, true);		
		
		final ExportXMLData data = new ExportXMLData();
		data.setExportType(ExportXMLData.EXPORT_METHOD_LIBRARY);
		data.setXMLFile(exportFile.getAbsolutePath());
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				(new ExportXMLService(data)).doExport(new NullProgressMonitor());
			}
		});
		
		return null;
	}
}
