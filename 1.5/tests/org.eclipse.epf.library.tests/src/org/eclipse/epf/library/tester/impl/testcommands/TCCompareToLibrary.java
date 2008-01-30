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

import org.eclipse.epf.importing.services.ConfigurationImportService;
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
public class TCCompareToLibrary extends TestCommandImpl {					
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_GoldenLibFolderName, element.getAttribute(AT_GoldenLibFolderName));
		setAttribute(AT_DiffCount, element.getAttribute(AT_DiffCount));
		setAttribute(AT_ElementCount, element.getAttribute(AT_ElementCount));
		setAttribute(AT_UseNameAsId, element.getAttribute(AT_UseNameAsId));
	}	
	
	public TCExeReply execute() {
		String goldenLibFolderName = getAttribute(AT_GoldenLibFolderName);
		int diffCount = 0;
		int elementCount = 0;
		boolean useNameAsId = false;
		
		String str = getAttribute(AT_DiffCount);
		if (str != null && str.length() > 0) {
			diffCount = Integer.parseInt(str);
		}
		str = getAttribute(AT_ElementCount);
		if (str != null && str.length() > 0) {
			elementCount = Integer.parseInt(str);
		}		
		str = getAttribute(AT_UseNameAsId);
		if (str != null && str.length() > 0) {
			useNameAsId = str.equalsIgnoreCase("true");
		}	
		
		TestCommandMgr owner = getOwner();
		MethodLibrary goldenLib  = owner.loadNonBaseLib(goldenLibFolderName);
		MethodLibrary curLib  = owner.getCurrentBaseLib();	
		if (useNameAsId) {
			ConfigurationImportService.fixImportLibrarySystemPackageGUIDs(goldenLib, curLib);
		}
		boolean result = owner.compareLibs(goldenLib, curLib, diffCount, elementCount, useNameAsId);
		return new TCExeReplyImpl("", result);
	}
}
