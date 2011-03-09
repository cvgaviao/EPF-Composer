/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2011. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.library.ui.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;

public class GenerateReportSelectScopePage extends WizardPage {
	
    protected GenerateReportSelectScopePage(String pageName) {
    	super(pageName);
    }

	public void createControl(Composite parent) {
		
		setControl(parent);
	}

}
