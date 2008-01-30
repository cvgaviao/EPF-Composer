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

import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCOpenLibrary extends TestCommandImpl {				
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_LibFolderName, element.getAttribute(AT_LibFolderName));
	}	
	
	public TCExeReply execute() {
		String libFolderName = getAttribute(AT_LibFolderName);	
		getOwner().loadBaseLib(libFolderName);
		return null;
	}
}