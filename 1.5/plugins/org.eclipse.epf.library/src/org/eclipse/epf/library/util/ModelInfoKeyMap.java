//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.util;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.LibraryResources;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.provider.UmaEditPlugin;


/**
 * class to map model info values to model info key values
 * 
 * @author Weiping Lu
 * @since 1.5
 *
 */
public class ModelInfoKeyMap {
	
	private static ModelInfoKeyMap instance = new ModelInfoKeyMap();
	private Map<String, String> map = new HashMap<String, String>();
	
	public ModelInfoKeyMap() {
		map.put(getString("_UI_Task_mandatoryInput_feature"), "mandatoryInput");//$NON-NLS-1$ //$NON-NLS-2$ 
		map.put(getString("_UI_Task_optionalInput_feature"), "optionalInput");//$NON-NLS-1$ //$NON-NLS-2$ 
		map.put(getString("_UI_TaskDescriptor_externalInput_feature"), "externalInput");//$NON-NLS-1$ //$NON-NLS-2$ 
		map.put(getString("_UI_Task_output_feature"), "output");//$NON-NLS-1$ //$NON-NLS-2$ 
		map.put(getString("_UI_TaskDescriptor_performedPrimarilyBy_feature"), "performedPrimarilyBy");//$NON-NLS-1$ //$NON-NLS-2$ 
		map.put(getString("_UI_TaskDescriptor_additionallyPerformedBy_feature"), "additionallyPerformedBy");//$NON-NLS-1$ //$NON-NLS-2$ 
		map.put(getString("_UI_TaskDescriptor_assistedBy_feature"), "assistedBy");//$NON-NLS-1$ //$NON-NLS-2$ 

		map.put(getString(TngUtil.getFeatureText(UmaPackage.eINSTANCE.getRoleDescriptor_ResponsibleFor())), 
				"ResponsibleFor");//$NON-NLS-1$  
		map.put(getString(TngUtil.getFeatureText(UmaPackage.eINSTANCE.getRoleDescriptor_Modifies())), 
				"Modifies");//$NON-NLS-1$ 
		map.put(getString(LibraryResources.ActivityLayout_primaryTasks_text), 
				"primaryTasks");//$NON-NLS-1$ 
		map.put(getString(LibraryResources.ActivityLayout_additionalTasks_text), 
				"additionalTasks");//$NON-NLS-1$
		map.put(getString(LibraryResources.ActivityLayout_assistTasks_text), 
				"assistTasks");//$NON-NLS-1$ 
		

	}
	
	public static ModelInfoKeyMap getInstance() {
		return instance;
	}
		
	private String getString(String uiKey) {
		return UmaEditPlugin.INSTANCE.getString(uiKey);
	}
	
	public String getModelInfoKey(String ModelInfo) {
		String key = "";		//$NON-NLS-1$ 
		
		return key;
	}
		
/*	
    Mandatory Input
    Optional Input
    External Input
    Output

    Primary Performer
    Additional Performer
    Assisted By

    ActivityLayout_primaryTasks_text=Performs as Owner
	ActivityLayout_additionalTasks_text=Performs as Additional
	ActivityLayout_assistTasks_text=Performs as assist

    Responsible for
    Modifies
*/
	
}
