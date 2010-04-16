/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.authoring.ui.properties;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Display;

public class SyncFreeLabelProvider extends AdapterFactoryLabelProvider implements ITableFontProvider {
	private FontRegistry registry = new FontRegistry();
	private DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
	
	private Font systemFont;
	private Descriptor desc;
	private EReference ref;
	private MethodConfiguration config;

	public SyncFreeLabelProvider(AdapterFactory adapterFactory, Descriptor desc, EReference ref, MethodConfiguration config) {
		super(adapterFactory);
		this.desc = desc;
		this.ref = ref;		
		this.config = config;
	}
    
    public Font getFont(Object obj, int columnIndex) {
    	if (systemFont == null) {
    		systemFont = Display.getCurrent().getSystemFont();
    	}
    	
    	if (propUtil.isDynamic(obj, desc, ref)) {
    		return registry.getBold(systemFont.getFontData()[0].getName());    		
    	}
    	
    	return systemFont;
    }	    	
    
    public String getColumnText(Object obj, int columnIndex) {
    	String original = super.getColumnText(obj, columnIndex);
    	
    	if (propUtil.isDynamicAndExclude(obj, desc, ref, config)) {
    		return "<<<" + original + ">>>";	    		 //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	
    	return original;	    	
    }
    
}