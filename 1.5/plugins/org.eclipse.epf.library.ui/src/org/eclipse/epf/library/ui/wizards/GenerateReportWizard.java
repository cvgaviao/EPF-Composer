/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2011. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.library.ui.wizards;

import org.eclipse.epf.library.ui.LibraryUIResources;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;

public class GenerateReportWizard extends Wizard implements IWorkbenchWizard {
	private IWorkbench workbench;
	private IStructuredSelection selection;
	
	private GenerateReportSelectScopePage selectScope;
	
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
		this.setWindowTitle(LibraryUIResources.generteReportWizard_title);
	}

	public boolean performFinish() {
		return false;
	}
	
    public void addPages() {
    	selectScope = new GenerateReportSelectScopePage(GenerateReportSelectScopePage.class.getName());
    	addPage(selectScope);
    }

}
