//------------------------------------------------------------------------------
// Copyright (c) 2005, 20087 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.configuration;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.library.configuration.closure.IConfigurationError;
import org.eclipse.epf.library.edit.util.MethodElementPropertyHelper;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElementProperty;

/**
 *  Class managing cached configuration properties
 * 
 * @author Weiping Lu - Mar 19, 2008
 * @since 1.5
 */
public class ConfigurationProperties {

	private MethodConfiguration config;
	private boolean hideWarnings = false;
	private boolean hideErrors = false;
	private boolean hideInfos = false;
	private boolean notifyingListeners = true;
	private List<Listener> listeners = new ArrayList<Listener>();
	
	public ConfigurationProperties(MethodConfiguration config) {
		this.config = config;
		loadFromConfiguration();
	}
			
	private String[] getHidePropStrings() {
		String[] hideProps = { MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_ERRORS,
				MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_WARNINGS,
				MethodElementPropertyHelper.CONFIG_PROPBLEM_HIDE_INFOS};
		return hideProps;
	}
	
	public void loadFromConfiguration() {
		String[] hideProps = getHidePropStrings();
		
		boolean oldNotifyingListeners = setNotifyListeners(false);
		for (int i = 0; i < hideProps.length; i++) {
			MethodElementProperty prop = MethodElementPropertyHelper.getProperty(config, hideProps[i]);
			String value = prop == null ? Boolean.FALSE.toString() : prop.getValue();
			boolean b = Boolean.TRUE.toString().equals(value);
			if (i == 0) {
				setHideErrors(b);
			} else if (i == 1) {
				setHideWarnings(b);
			} else if (i == 2) {
				setHideInfos(b);
			}
		}
		setNotifyListeners(oldNotifyingListeners);
	}
	
	public boolean toHide(IConfigurationError error) {
		if (error.isError()) {
			return isHideErrors();
		} else if (error.isWarning()) {
			return isHideWarnings();
		} else {
			return isHideInfos();
		}
	}

	public boolean isHideWarnings() {
		return hideWarnings;
	}
	public void setHideWarnings(String value) {
		setHideWarnings(Boolean.TRUE.toString().equals(value));
	}
	public void setHideWarnings(boolean hideWarnings) {
		if (this.hideWarnings != hideWarnings) {
			this.hideWarnings = hideWarnings;
			notifyListeners();
		}
	}
	
	public boolean isHideErrors() {
		return hideErrors;
	}
	public void setHideErrors(String value) {
		setHideErrors(Boolean.TRUE.toString().equals(value));
	}
	public void setHideErrors(boolean hideErrors) {
		if (this.hideErrors != hideErrors) {
			this.hideErrors = hideErrors;
			notifyListeners();
		}
	}

	public boolean isHideInfos() {
		return hideInfos;
	}
	public void setHideInfos(String value) {
		setHideInfos(Boolean.TRUE.toString().equals(value));
	}
	public void setHideInfos(boolean hideInfos) {
		if (this.hideInfos != hideInfos) {
			this.hideInfos = hideInfos;
			notifyListeners();
		}
	}

	public void addListeners(Listener listener) {
		this.listeners.add(listener);
	}
	
	public void removeListeners(Listener listener) {
		this.listeners.remove(listener);
	}
	
	private void notifyListeners() {
		if (! getNotifyingListeners()) {
			return;
		}
		for (Listener listener: listeners) {
			listener.fireEvent();
		}
	}
	
	public static class Listener {
		public void fireEvent() {			
		}
	}
	
	private boolean getNotifyingListeners() {
		return notifyingListeners;
	}
	
	//return old value
	public boolean setNotifyListeners(boolean b) {
		boolean oldValue = notifyingListeners;
		notifyingListeners = b;
		return oldValue;
	}
	
}
