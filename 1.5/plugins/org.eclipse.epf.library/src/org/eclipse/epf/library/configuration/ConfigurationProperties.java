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

import org.eclipse.epf.library.configuration.closure.IConfigurationError;
import org.eclipse.epf.uma.MethodConfiguration;

/**
 *  Class managing configuration properties
 * 
 * @author Weiping Lu - Mar 19, 2008
 * @since 1.5
 */
public class ConfigurationProperties {

	private MethodConfiguration config;
	private boolean hideWarnings = false;
	private boolean hideErrors = false;
	private boolean hideInfos = false;
	
	public ConfigurationProperties(MethodConfiguration config) {
		this.config = config;
		loadFromConfiguration();
	}
	
	private void loadFromConfiguration() {
		//tbi
	}
	
	public void saveToConfiguration() {
		//tbi
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
	public void setHideWarnings(boolean hideWarnings) {
		this.hideWarnings = hideWarnings;
	}
	
	public boolean isHideErrors() {
		return hideErrors;
	}
	public void setHideErrors(boolean hideErrors) {
		this.hideErrors = hideErrors;
	}

	public boolean isHideInfos() {
		return hideInfos;
	}
	public void setHideInfos(boolean hideInfos) {
		this.hideInfos = hideInfos;
	}

}
