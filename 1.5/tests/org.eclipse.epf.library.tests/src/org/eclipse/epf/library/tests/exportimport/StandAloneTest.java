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

import java.io.File;

import junit.framework.TestCase;

import org.eclipse.epf.importing.services.ConfigurationImportService;
import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.eclipse.epf.uma.MethodLibrary;

/**
 * JUnit tests for plugin export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class StandAloneTest extends TestCase {

	private LibraryTestService service = LibraryTestService.getInstance();
	
	public StandAloneTest() {
	}	
	
	public void test0001() {

 		//LibraryDiffAnalyzor.setTrim(true);
		//LibraryDiffAnalyzor.setUseNameAs2ndId(true);
		//LibraryDiffAnalyzor.addExcludedFeature("version");
		//LibraryDiffAnalyzor.addExcludedFeature("name");
		//LibraryDiffAnalyzor.addExcludedFeature("presentationName");
		//LibraryDiffAnalyzor.addExcludedFeature("mainDescription");
		//LibraryDiffAnalyzor.addExcludedFeature("sectionDescription");	
		
		TestCommandMgr tcMgr = new TestCommandMgr(true);
		File file = new File("C:\\Libs\\72\\");
		tcMgr.openCurrTestDir(file);
		
		String lib1Path = "C:\\Libs\\72\\OpenUp72";
		String lib2Path = "C:\\Libs\\72\\a";		
		MethodLibrary lib1 = service.loadLibrary(lib1Path);
		MethodLibrary lib2 = service.loadLibrary(lib2Path);
		ConfigurationImportService.fixImportLibrarySystemPackageGUIDs(lib1, lib2);
		tcMgr.trace("Comparing libraries -> Begin");
		tcMgr.trace("lib1: " + lib1Path);
		tcMgr.trace("lib2: " + lib2Path);

		boolean result = tcMgr.compareLibs(lib1, lib2, 0, 0, true);
		assertTrue(result);
		tcMgr.trace("Comparing libraries <- End ");
	}		
			
	
}
