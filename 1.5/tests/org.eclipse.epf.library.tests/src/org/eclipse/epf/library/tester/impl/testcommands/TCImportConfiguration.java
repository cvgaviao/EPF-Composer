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
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.importing.services.ConfigurationImportData;
import org.eclipse.epf.importing.services.ConfigurationImportService;
import org.eclipse.epf.importing.services.ElementDiffTree;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.ProcessComponent;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCImportConfiguration extends TestCommandImpl {				

	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ImportFolderName, element.getAttribute(AT_ImportFolderName));
	}	
	
	public TCExeReply execute() {
		final TestCommandMgr owner = getOwner();
		String importFolderName = getAttribute(AT_ImportFolderName);
		
		File file = owner.getImportFile(importFolderName);
		if (file == null) {
			return null;
		}
		
		ConfigurationImportData data = new ConfigurationImportData();
		data.llData.setLibName(importFolderName);
		data.llData.setParentFolder(file.getAbsolutePath());	

		final ConfigurationImportService importService = new ConfigurationImportService(data);
		importService.analyze(new NullProgressMonitor());

		Object[] elements = getElements(importService.getDiffTree());
		data.importList.clear();
		for ( int i = 0; i < elements.length; i++ ) {
			data.importList.add(elements[i]);
		}
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				try {
					importService.performImport(new NullProgressMonitor());
					MethodLibrary lib  = org.eclipse.epf.library.LibraryService.getInstance().getCurrentMethodLibrary();
					owner.setCurrentBaseLib(lib);
				} catch (Throwable e) {
					e.printStackTrace();
				}
			}
		});
		
		return null;
	}
	
	private boolean selectable(ElementDiffTree node) {
		if (node instanceof ProcessComponent) {
			return false;
		}
		if (node.isOldOnly()) {
			return false;
		}
		MethodElement base = node.getBaseElement();
		if ( (base instanceof MethodPackage) 
				&& TngUtil.isRootCutomCategoryPackage((MethodPackage)base) ) {
			return false;
		}
		return true;
	}
	
	private void selectNodes(ElementDiffTree node, ArrayList selectedNodes) {
		if (! selectable(node)) {
			return;
		}
		selectedNodes.add(node);
		List cnodes = node.getChildren();
		int sz = cnodes == null ? 0 : cnodes.size();
		for (int i=0; i<sz; i++) {
			ElementDiffTree cnode = (ElementDiffTree) cnodes.get(i);
			selectNodes(cnode, selectedNodes);
		}
	}
	
	private Object[] getElements(ElementDiffTree root) {
		ArrayList selectedNodes = new ArrayList();
		selectNodes(root, selectedNodes);
		return selectedNodes.toArray();
	}
	
}



