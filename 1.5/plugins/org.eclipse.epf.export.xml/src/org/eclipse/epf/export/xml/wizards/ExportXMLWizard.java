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
package org.eclipse.epf.export.xml.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.epf.common.ui.util.MsgBox;
import org.eclipse.epf.export.services.ConfigurationExportData;
import org.eclipse.epf.export.services.PluginExportData;
import org.eclipse.epf.export.xml.ExportXMLPlugin;
import org.eclipse.epf.export.xml.ExportXMLResources;
import org.eclipse.epf.export.xml.preferences.ExportXMLPreferences;
import org.eclipse.epf.export.xml.services.ExportXMLData;
import org.eclipse.epf.export.xml.services.ExportXMLService;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;

/**
 * The Export XML wizard.
 * <p>
 * This wizard is used to export method library content to XML files.
 * 
 * @author Jinhua Xi
 * @author Kelvin Low
 * @since 1.0
 */
public class ExportXMLWizard extends Wizard implements IImportWizard {

	private SelectExportTypePage selectExportTypePage;

	protected SelectPluginPage selectPluginPage;

	protected ViewPluginInfoPage viewPluginInfoPage;

	protected ViewExportSummaryPage viewExportSummaryPage;

	protected SelectConfigPage selectConfigPage;

	protected SelectXMLFilePage selectXMLFilePage;

	protected ExportXMLData xmlData = new ExportXMLData();

	protected PluginExportData pluginData = new PluginExportData();

	protected ConfigurationExportData configData = new ConfigurationExportData();

	/**
	 * Creates a new instance.
	 */
	public ExportXMLWizard() {
		super();
		setWindowTitle(ExportXMLResources.exportXMLWizard_title);
		setNeedsProgressMonitor(true);
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchWizard#init(IWorkbench,
	 *      IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
	}

	/**
	 * @see org.eclipse.jface.wizard.Wizard#addPages()
	 */
	public void addPages() {
		selectExportTypePage = new SelectExportTypePage();
		selectPluginPage = new SelectPluginPage(pluginData);
		viewPluginInfoPage = new ViewPluginInfoPage(pluginData);
		viewExportSummaryPage = new ViewExportSummaryPage(pluginData);
		selectConfigPage = new SelectConfigPage(configData);
		selectXMLFilePage = new SelectXMLFilePage();

		addPage(selectExportTypePage);
		addPage(selectPluginPage);
		addPage(viewPluginInfoPage);
		addPage(viewExportSummaryPage);
		addPage(selectConfigPage);
		addPage(selectXMLFilePage);
	}

	/**
	 * @see org.eclipse.jface.wizard.Wizard#createPageControls(Composite)
	 */
	public void createPageControls(Composite pageContainer) {
		super.createPageControls(pageContainer);
		pageContainer.getShell().setImage(
				ExportXMLPlugin.getDefault().getSharedImage(
						"full/obj16/XMLFile.gif")); //$NON-NLS-1$
	}

	/**
	 * @see org.eclipse.jface.wizard.IWizard#canFinish()
	 */
	public boolean canFinish() {
		return getContainer().getCurrentPage() == selectXMLFilePage
				&& selectXMLFilePage.isPageComplete();
	}

	/**
	 * @see org.eclipse.jface.wizard.IWizard#performFinish()
	 */
	public boolean performFinish() {
		String xmlFilePath = selectXMLFilePage.getPath();
		File xmlFile = new File(xmlFilePath);
		if (xmlFile.exists()) {
			boolean ok = ExportXMLPlugin.getDefault().getMsgDialog()
					.displayPrompt(
							ExportXMLResources.exportXMLWizard_title,
							ExportXMLResources.bind(
									ExportXMLResources.overwriteText_msg,
									new String[] { xmlFilePath }));
			if (!ok) {
				return false;
			}
		}

		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor)
					throws InvocationTargetException {
				ExportXMLService service = null;
				try {
					monitor.beginTask(ExportXMLResources.exportingXML_text,
							IProgressMonitor.UNKNOWN);

					xmlData.setXMLFile(selectXMLFilePage.getPath());
					service = new ExportXMLService(xmlData);
					service.doExport(monitor);
					ExportXMLPreferences.setExportType(xmlData.getExportType());
					ExportXMLPreferences.setXMLFile(xmlData.getXMLFile());
				} catch (Exception e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
					if (service != null) {
						service.dispose();
					}
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			ExportXMLPlugin.getDefault().getMsgDialog().displayError(
					ExportXMLResources.exportXMLWizard_title,
					NLS.bind(ExportXMLResources.exportXMLWizard_error,
							realException.getMessage()), realException);
			return false;
		}

		String msg = ExportXMLResources.exportXMLWizard_reviewLog;
		MsgBox.prompt(msg, SWT.OK);

		return true;
	}

}
