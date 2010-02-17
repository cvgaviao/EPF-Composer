package org.eclipse.epf.toolbox.batch.commands;

import java.io.File;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.epf.export.xml.services.ExportXMLData;
import org.eclipse.epf.export.xml.services.ExportXMLService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

public class EbcExportXml extends EpfBatchCommandImpl {

	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.XmlExportFilePath, element.getAttribute(C.XmlExportFilePath));
	}	
	
	public EbcExeReply execute() {
		traceBeforeExcute();	
		final EbcExeReply reply = new EbcExeReply();				
		
		try {
			executeBody(reply);
		} catch (Exception e) {
			reply.addStatus(Status.ERROR, e.getMessage(), e);
		}

		traceAfterExcute(reply);		
		return reply;
	}
	
	
	
	public void executeBody(EbcExeReply reply) {
				
		String relPath = getAttribute(C.XmlExportFilePath);
		
		MethodLibrary lib = getMgr().getCurrentBaseLib();
		File file = new File(lib.eResource().getURI().toFileString());
		file = file.getParentFile().getParentFile();
		
		String absPath = getMgr().getRootPath() + File.separator + relPath;
		File exportFile = new File(absPath);		
		
		final ExportXMLData data = new ExportXMLData();
		data.setExportType(ExportXMLData.EXPORT_METHOD_LIBRARY);
		data.setXMLFile(exportFile.getAbsolutePath());
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				(new ExportXMLService(data)).doExport(new NullProgressMonitor());
			}
		});
		
	}
	
}
