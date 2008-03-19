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

import org.eclipse.epf.uma.MethodConfiguration;

/**
 *  Class managing configuration properties
 * 
 * @author Weiping Lu - Mar 19, 2008
 * @since 1.5
 */
public class ConfigurationProperties {

	private MethodConfiguration config;
	
	public ConfigurationProperties(MethodConfiguration config) {
		this.config = config;
		loadFromConfiguration();
	}
	
	public boolean getBooleanValue(String propertyName) {
		return true;
	}
	
	public int getIntValue(String propertyName) {
		return 0;
	}
	
	public String getStringValue(String propertyName) {
		return null;
	}
	
	public void setBooleanValue(String propertyName, boolean value) {
		
	}
	
	public void setInt(String propertyName, int value) {

	}
	
	public void setString(String propertyName, String value) {

	}
	
	private void loadFromConfiguration() {
		
	}
	
	public void saveToConfiguration() {
		
	}
	

}
