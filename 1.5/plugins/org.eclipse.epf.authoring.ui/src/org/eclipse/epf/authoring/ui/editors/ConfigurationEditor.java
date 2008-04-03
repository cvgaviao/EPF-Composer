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
package org.eclipse.epf.authoring.ui.editors;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.forms.ConfigViewPage;
import org.eclipse.epf.authoring.ui.forms.ConfigurationDescription;
import org.eclipse.epf.authoring.ui.forms.ConfigurationPage;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationProperties;
import org.eclipse.epf.persistence.refresh.RefreshJob;
import org.eclipse.epf.persistence.util.PersistenceUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IGotoMarker;
import org.eclipse.ui.part.FileEditorInput;


/**
 * The Method Configuration editor.
 * 
 * @author Shilpa Toraskar
 * @author Kelvin Low
 * @since 1.0
 */
public class ConfigurationEditor extends MethodElementEditor implements IGotoMarker {

	/**
	 * The editor ID.
	 */
	public static final String EDITOR_ID = ConfigurationEditor.class.getName();

	ConfigurationPage configPage = null;

	/**
	 * Creates a new instance.
	 */
	public ConfigurationEditor() {
		super();
	}

	/**
	 * Returns the method configuration associated with this editor.
	 */
	public MethodConfiguration getConfiguration() {
		return ((ConfigurationEditorInput) super.getEditorInput())
				.getConfiguration();
	}

	
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		if (input instanceof FileEditorInput) {
			// probably opened from Problems View
			// create a ConfigurationEditorInput
			Resource resource = RefreshJob.getInstance().getResource(((FileEditorInput)input).getFile());
			MethodElement element = PersistenceUtil.getMethodElement(resource);
			if (element instanceof MethodConfiguration) {
				input = new ConfigurationEditorInput((MethodConfiguration)element);
			}
		}

		super.init(site, input);
	}
	/**
	 * @see org.eclipse.ui.forms.editor.FormEditor#addPages()
	 */
	protected void addPages() {
		try {
			addPage(new ConfigurationDescription(this));
			configPage = new ConfigurationPage(this);
			addPage(configPage);
			addPage(new ConfigViewPage(this));
		} catch (PartInitException e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.editors.MethodElementEditor#dispose()
	 */
	public void dispose() {
		if (isDirty()) {
			ConfigurationProperties props = LibraryService.getInstance().getConfigurationManager(getConfiguration()).getConfigurationProperties();
			if (props.isDirty()) {
				props.loadFromConfiguration();
			}
		}

		super.dispose();
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.editors.MethodElementEditor#createInput(org.eclipse.epf.uma.MethodElement)
	 */
	protected IEditorInput createInput(MethodElement e) {
		if(e instanceof MethodConfiguration) {
			return new ConfigurationEditorInput((MethodConfiguration) e);
		}
		return null;
	}
	
	private List<Listener> setFocusListeners = new ArrayList<Listener>();
	
	
	/**
	 * Add given listener to list of focus listeners
	 * @param lis
	 */
	public void addToSetFocusLiseners(Listener lis) {
		setFocusListeners.add(lis);
	}
	
    /**
     * @see org.eclipse.ui.part.MultiPageEditorPart#setFocus()
     */
    public void setFocus() {    	
    	Event e = new Event();
    	e.data = getActivePageInstance();   	
    	for (int i=0; i<setFocusListeners.size(); i++) {
    		Listener lis = (Listener) setFocusListeners.get(i);
    		lis.handleEvent(e);
    	}
    }

    public void gotoMarker(IMarker marker) {
    	// activate config page
    	setActivePage(1);
    	if (configPage != null) {
    		configPage.gotoMarker(marker);
    	}
    }
    
    public void doQuickFix(IMarker marker) {
    	configPage.doQuickFix(marker);
    }
    
	public Collection getModifiedResources() {
		Collection col = super.getModifiedResources();
		Resource resource = getConfiguration().eResource();
		if (isDirty() && !col.contains(resource)) {
			ConfigurationProperties props = LibraryService.getInstance().getConfigurationManager(getConfiguration()).getConfigurationProperties();
			if (props.isDirty()) {
				props.saveToConfiguration();
				col.add(resource);
			}
		}
		return col;
	}
    
}
