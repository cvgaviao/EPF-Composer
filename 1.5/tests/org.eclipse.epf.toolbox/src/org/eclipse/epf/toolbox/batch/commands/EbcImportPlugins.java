package org.eclipse.epf.toolbox.batch.commands;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.epf.importing.services.PluginImportData;
import org.eclipse.epf.importing.services.PluginImportingService;
import org.eclipse.epf.importing.services.PluginImportData.PluginInfo;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class EbcImportPlugins extends EpfBatchCommandImpl {

	private Set<String> selectedPlugins;

	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.ImportAll, element.getAttribute(C.ImportAll));
		setAttribute(C.ImportFolderPath, element
				.getAttribute(C.ImportFolderPath));

		boolean importAll = parseBoolean(C.ImportAll, false);

		if (!importAll) {
			selectedPlugins = new HashSet<String>();
			NodeList nodes = element.getChildNodes();
			int sz = nodes == null ? 0 : nodes.getLength();
			for (int i = 0; i < sz; i++) {
				Node node = nodes.item(i);
				if (node instanceof Element) {
					Element cElement = (Element) node;
					if (cElement.getTagName().equals(C.SelectedPlugin)) {
						String name = cElement.getAttribute(C.Name);
						selectedPlugins.add(name);
					}
				}
			}
		}
	}

	protected void executeBody(final EbcExeReply reply) {
		
		WorkspaceModifyOperation operation = new WorkspaceModifyOperation() {
			public void execute(IProgressMonitor monitor) {
				try {
					boolean importAll = parseBoolean(C.ImportAll, false);
					
					String importLibPath = getAttribute(C.ImportFolderPath);
					File importLibFolder = new File(importLibPath);

					PluginImportData data = new PluginImportData();
					data.llData.setLibName(importLibFolder.getName());
					data.llData.setParentFolder(importLibFolder
							.getAbsolutePath());
					final PluginImportingService service = new PluginImportingService(
							data);
					service.validate(null);

					List importPlugins = data.getPlugins();
					for (int i = 0; i < importPlugins.size(); i++) {
						PluginInfo info = (PluginInfo) importPlugins
								.get(i);
						info.selected = importAll || selectedPlugins.contains(info.name);
					}
					service.performImport(monitor);
				} catch (Exception e) {
					reply.addStatus(Status.ERROR, e.getMessage(), e);
				} finally {
					monitor.done();
				}
			}
		};

		try {
			ProgressMonitorDialog pmDialog = new ProgressMonitorDialog(
					Display.getCurrent().getActiveShell());
			pmDialog.run(true, false, operation);
		} catch (Exception e) {
			reply.addStatus(Status.ERROR, e.getMessage(), e);
		}
	}
}
