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
package org.eclipse.epf.library.tester.iface;

import org.w3c.dom.Element;

/**
 * Used in JUnit tests 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public interface TestCommand {				
	
	String TestNameExt = ".tc";
	
	//All the tags will be formally specified and read from
	//a schema later.
	
	//Attribute tags
	String AT_ExtId = "extId";
	String AT_Disable = "disable";
	String AT_LibFolderName = "libFolderName";
	String AT_ExportFolderName = "exportFolderName";
	String AT_ImportFolderName = "importFolderName";
	String AT_GoldenLibFolderName = "goldenLibFolderName";
	String AT_SelectedConfigName = "selectedConfigName";
	String AT_DiffCount = "diffCount";
	String AT_ElementCount = "elementCount";
	String AT_UseNameAsId= "useNameAsId";
	String AT_Merge = "merge";
	String AT_Name = "name";
	String AT_Type = "type";
	String AT_BriefDescription = "briefDescription";
	String AT_Authors = "authors";
	String AT_ParentPath="parentPath";
	String AT_ParentExtId="parentExtId";
	String AT_Path="path";
	String AT_Recursive="recursive";
	String AT_GoldenFileName = "goldenFileName";
	String AT_OutputFileName = "outputFileName";
	String AT_ErrorCount = "errorCount";
	String AT_CircularElementCount = "circularElementCount";
	String AT_Verbose = "verbose";
		
	//Value element tags
	String VT_bases = "bases";
	String VT_Value = "Value";
				
	String tagName();
	//LibraryTester getTester();	
	void setAttribute(String attName, String attValue);
	String getAttribute(String attName);
	void parse(Element element);
	TCExeReply execute();
}
