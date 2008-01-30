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

import java.util.List;

import org.eclipse.epf.authoring.ui.PerspectiveListUtil;
import org.eclipse.epf.authoring.ui.UIActionDispatcher;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.MethodConfiguration;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCNewMethodConfiguration extends TestCommandImpl {				
	
	private List referencedPlugins;
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_Name, element.getAttribute(AT_Name));
		setAttribute(AT_BriefDescription, element.getAttribute(AT_BriefDescription));
	}	
	
	public TCExeReply execute() {
		
		String name = getAttribute(AT_Name);
		String briefDescription = getAttribute(AT_BriefDescription);
		
		MethodConfiguration config = null;
		
		name = name.trim();
		if (name != null) {
			try {
				config = LibraryService.getInstance()
						.createMethodConfiguration(
								name,
								LibraryService.getInstance()
										.getCurrentMethodLibrary());
				config.setBriefDescription(briefDescription);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		if (!PerspectiveListUtil.isAuthoringPerspective()) {
			UIActionDispatcher.openAuthoringPerspective();
		}
		ILibraryPersister.FailSafeMethodLibraryPersister persister = LibraryServiceUtil.getCurrentPersister().getFailSafePersister();
		try {
			persister.save(config);
		} catch (Exception e) {
			e.printStackTrace();
		}		
		
		return null;
	}
	
}