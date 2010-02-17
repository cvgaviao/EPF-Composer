package org.eclipse.epf.toolbox.batch.commands;

import java.io.File;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.importing.xml.services.ImportXMLService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

public class EbcImportXml extends EpfBatchCommandImpl {

	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.XmlImportFilePath, element.getAttribute(C.XmlImportFilePath));
	}	
	
	protected void executeBody(final EbcExeReply reply) {

		boolean merge = true;		
		String relPath = getAttribute(C.XmlImportFilePath);
		String absPath = getMgr().getRootPath() + File.separator + relPath;
		
		final File file = new File(absPath);
		
		final boolean overwrite = ! merge;
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				try {
					ImportXMLService importService = new ImportXMLService();
					importService.setOverwrite(overwrite);
					importService.loadXml(file.getAbsolutePath());					
					importService.doImport(new NullProgressMonitor());
					MethodLibrary lib  = org.eclipse.epf.library.LibraryService.getInstance().getCurrentMethodLibrary();
					getMgr().setCurrentBaseLib(lib);
				} catch (Throwable e) {
					e.printStackTrace();
				}
			}
		});
		

	}
	
	
}
