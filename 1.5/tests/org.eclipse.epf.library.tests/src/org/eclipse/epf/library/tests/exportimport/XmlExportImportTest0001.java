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
package org.eclipse.epf.library.tests.exportimport;

import org.eclipse.epf.library.tester.LibraryJunitTestService;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.iface.TestCommand;
import org.eclipse.epf.library.tester.impl.ExportImportTestImpl;
import org.eclipse.epf.library.tester.impl.testcommands.TCCompareToLibrary;
import org.eclipse.epf.library.tester.impl.testcommands.TCCopyLibrary;
import org.eclipse.epf.library.tester.impl.testcommands.TCExportXml;
import org.eclipse.epf.library.tester.impl.testcommands.TCImportXml;
import org.eclipse.epf.library.tester.impl.testcommands.TCOpenLibrary;

/**
 * JUnit tests for plugin export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class XmlExportImportTest0001 extends ExportImportTestMethodBase {
	
	public XmlExportImportTest0001(ExportImportTestImpl owner) {
		super(owner);
	}
	
	public boolean execute(String testName) {
		LibraryJunitTestService service = getOwner().getService();
		
		TestCommand tcs[] = new TestCommand[6];
		
		tcs[0] = service.newTestCommand(TCCopyLibrary.class);
		tcs[0].setAttribute(TestCommand.AT_LibFolderName, "Paris");
		
		tcs[1] = service.newTestCommand(TCOpenLibrary.class);
		tcs[1].setAttribute(TestCommand.AT_LibFolderName, "Paris");
		
		tcs[2] = service.newTestCommand(TCExportXml.class);
		tcs[2].setAttribute(TestCommand.AT_ExportFolderName, "Export0001");
		
		tcs[3] = service.newTestCommand(TCOpenLibrary.class);
		tcs[3].setAttribute(TestCommand.AT_LibFolderName, "Base_1");
		
		tcs[4] = service.newTestCommand(TCImportXml.class);
		tcs[4].setAttribute(TestCommand.AT_ImportFolderName, "Export0001");
		
		tcs[5] = service.newTestCommand(TCCompareToLibrary.class);
		tcs[5].setAttribute(TestCommand.AT_GoldenLibFolderName, "Paris");		
		tcs[5].setAttribute(TestCommand.AT_DiffCount, "28");
		tcs[5].setAttribute(TestCommand.AT_ElementCount, "370");
		tcs[5].setAttribute(TestCommand.AT_UseNameAsId, "true");
		
		boolean passing = true;
		for (int i=0; i<tcs.length; i++) {
			TCExeReply reply = tcs[i].execute();
			if (reply != null && ! reply.passing()) {
				passing = false;
			}				
		}
		getOwner().assertTrue(passing);
		
		return passing;
	}			
	
}
