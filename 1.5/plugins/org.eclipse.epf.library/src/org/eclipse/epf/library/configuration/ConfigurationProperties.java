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
import org.eclipse.epf.library.edit.util.MethodElementPropertyHelper;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElementProperty;

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
	private static final String trueValue = "true";	//$NON-NLS-1$
	private static final String falseValue = "false";	//$NON-NLS-1$
	
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
	
	private void loadFromConfiguration() {
		String[] hideProps = getHidePropStrings();
		for (int i = 0; i < hideProps.length; i++) {
			MethodElementProperty prop = MethodElementPropertyHelper.getProperty(config, hideProps[i]);
			String value = prop == null ? falseValue : prop.getValue();
			boolean b = trueValue.equals(value);
			if (i == 0) {
				setHideErrors(b);
			} else if (i == 1) {
				setHideWarnings(b);
			} else if (i == 2) {
				setHideWarnings(b);
			}
		}
	}
	
	public void saveToConfiguration() {
		String[] hideProps = getHidePropStrings();
		for (int i = 0; i < hideProps.length; i++) {
			String value = null;
			MethodElementProperty prop = MethodElementPropertyHelper.getProperty(config, hideProps[i]);
			String oldValue = prop == null ? falseValue : prop.getValue();
			boolean oldB = trueValue.equals(oldValue);
			if (i == 0) {
				if (oldB != isHideErrors()) {
					value = isHideErrors() ? trueValue : falseValue;
				}
			} else if (i == 1) {
				if (oldB != isHideWarnings()) {
					value = isHideWarnings() ? trueValue : falseValue;
				}
			} else if (i == 2) {
				if (oldB != isHideInfos()) {
					value = isHideInfos() ? trueValue : falseValue;
				}
			}
			if (value != null) {
				MethodElementPropertyHelper.setProperty(config, hideProps[i], value);
			}
		}
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
