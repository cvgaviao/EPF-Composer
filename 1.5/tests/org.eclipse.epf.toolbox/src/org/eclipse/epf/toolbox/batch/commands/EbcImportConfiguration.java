package org.eclipse.epf.toolbox.batch.commands;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.importing.services.ConfigurationImportData;
import org.eclipse.epf.importing.services.ConfigurationImportService;
import org.eclipse.epf.importing.services.ElementDiffTree;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.ProcessComponent;
import org.w3c.dom.Element;

public class EbcImportConfiguration extends EpfBatchCommandImpl {

	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.ImportFolderPath, element.getAttribute(C.ImportFolderPath));
	}	

	protected void executeBody(final EbcExeReply reply) {
		String relPath = getAttribute(C.ImportFolderPath);		
		String absPath = getMgr().getRootPath() + File.separator + relPath;		
		File file = new File(absPath);
		
		ConfigurationImportData data = new ConfigurationImportData();
		data.llData.setLibName(file.getName());
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
					getMgr().setCurrentBaseLib(lib);
				} catch (Throwable e) {
					e.printStackTrace();
				}
			}
		});
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
