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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.library.util.ModelStorage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.UmaFactory;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCNewMethodPlugin extends TestCommandImpl {				
	
	private List referencedPlugins;
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_Name, element.getAttribute(AT_Name));
		setAttribute(AT_BriefDescription, element.getAttribute(AT_BriefDescription));
		setAttribute(AT_Authors, element.getAttribute(AT_Authors));
		parseChildren(element);
	}
	
	protected void parseChildren(Element element) {
		NodeList cnodes = element.getElementsByTagName(VT_bases);
		if (cnodes == null || cnodes.getLength() == 0) {
			return;
		}
		Element cElement = (Element) cnodes.item(0);
		cnodes = cElement.getElementsByTagName(VT_Value);	
		if (cnodes == null || cnodes.getLength() == 0) {
			return;
		}
		referencedPlugins = new ArrayList();
		for (int i=0; i<cnodes.getLength(); i++) {
			Element vElem = (Element) cnodes.item(i);
			String value = vElem.getFirstChild().getNodeValue();
			referencedPlugins.add(value);
		}		
	}
	
	public TCExeReply execute() {
		
		String name = getAttribute(AT_Name);
		String briefDescription = getAttribute(AT_BriefDescription);
		String authors = getAttribute(AT_Authors);
		
		if (name == null || name.length() == 0) {
			return new TCExeReplyImpl("Empty plugin name", false);
		}
		String msg = LibraryUtil.checkPluginName(null, name);
		if (msg != null) {
			return new TCExeReplyImpl(msg, false);
		}
				
		try {
			MethodLibrary currLib = getOwner().getCurrentBaseLib();
			List plugins = currLib.getMethodPlugins();
			
			// Create the method plug-in and add it to the current method
			// library.
			final MethodPlugin newPlugin = UmaFactory.eINSTANCE.createMethodPlugin();
			newPlugin.setName(StrUtil.makeValidFileName(name));
			newPlugin.setBriefDescription(briefDescription);
			newPlugin.setAuthors(authors);
			
			int sz = referencedPlugins == null ? 0 : referencedPlugins.size();
			if (sz > 0 && ! plugins.isEmpty()) {
				for (int i=0; i<sz; i++) {
					String referedName = (String) referencedPlugins.get(i);
					MethodPlugin plugin = (MethodPlugin) LibraryTestService.getElement
						(plugins, referedName);
					if (plugin != null) {
						newPlugin.getBases().add(plugin);
					}
				}
			}

			ILibraryManager manager = (ILibraryManager) LibraryService
					.getInstance().getCurrentLibraryManager();
			if (manager != null) {
				manager.addMethodPlugin(ModelStorage.initialize(newPlugin));
			}

		} catch (Exception e) {
		}
		return null;
	}
	
}