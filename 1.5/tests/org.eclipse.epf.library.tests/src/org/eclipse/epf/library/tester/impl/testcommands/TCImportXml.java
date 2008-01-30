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
import org.eclipse.epf.importing.xml.services.ImportXMLService;
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
public class TCImportXml extends TestCommandImpl {					
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ImportFolderName, element.getAttribute(AT_ImportFolderName));
		setAttribute(AT_Merge, element.getAttribute(AT_Merge));
	}	
	
	public TCExeReply execute() {
		final TestCommandMgr owner = getOwner();
		String importFolderName = getAttribute(AT_ImportFolderName);
		boolean merge = false;
		String str = getAttribute(AT_Merge);
		if (str != null && str.equalsIgnoreCase("true")) {
			merge = false;
		}
		
		File file0 = owner.getImportFile(importFolderName);
		if (file0.isDirectory()) {
			String path = file0.getAbsolutePath() + File.separator + file0.getName() + ".xml";
			file0 = new File(path);
		}
		
		final File file = file0;
		if (file == null || ! file.exists()) {
			return null;
		}		
		
		final boolean overwrite = ! merge;
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				try {
					ImportXMLService importService = new ImportXMLService();
					importService.setOverwrite(overwrite);
					importService.loadXml(file.getAbsolutePath());					
					importService.doImport(new NullProgressMonitor());
					MethodLibrary lib  = org.eclipse.epf.library.LibraryService.getInstance().getCurrentMethodLibrary();
					owner.setCurrentBaseLib(lib);
				} catch (Throwable e) {
					e.printStackTrace();
				}
			}
		});
		
		return null;
	}
}
