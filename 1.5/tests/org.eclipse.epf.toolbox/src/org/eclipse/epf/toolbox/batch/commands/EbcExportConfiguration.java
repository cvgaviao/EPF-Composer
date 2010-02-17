package org.eclipse.epf.toolbox.batch.commands;

import java.io.File;
import java.util.ArrayList;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.epf.export.services.ConfigurationExportData;
import org.eclipse.epf.export.services.ConfigurationExportService;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

public class EbcExportConfiguration extends EpfBatchCommandImpl {

	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.ExportFolderPath, element.getAttribute(C.ExportFolderPath));
		setAttribute(C.SelectedConfigName, element.getAttribute(C.SelectedConfigName));
	}	

	protected void executeBody(EbcExeReply reply) {
		MethodLibrary currLib = getMgr().getCurrentBaseLib();
		if (currLib == null) {
			return;
		}
		
		String selectedConfigName = getAttribute(C.SelectedConfigName);		
		MethodConfiguration config = (MethodConfiguration) getMgr().getService().getElement
						(currLib.getPredefinedConfigurations(), selectedConfigName);				
		if (config == null) {
			return;
		}
		
		String path = getAttribute(C.ExportFolderPath);
		String exportFolderPath = getMgr().getRootPath() + File.separator + path;
		File exportFolder = new File(exportFolderPath);
		
		final ConfigurationExportData data = new ConfigurationExportData();
		data.llData.setLibName(currLib.getName());
		data.llData.setParentFolder(exportFolder.getAbsolutePath());
		data.exportOneConfig = true;
		data.exportConfigSpecs = false;
		data.selectedConfigs = new ArrayList();
		data.selectedConfigs.add(config);
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				(new ConfigurationExportService(data)).run(new NullProgressMonitor());
			}
		});
	}	
	
}
