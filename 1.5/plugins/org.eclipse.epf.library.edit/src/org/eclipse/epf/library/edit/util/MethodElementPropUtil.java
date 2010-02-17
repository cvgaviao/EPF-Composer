package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;

public class MethodElementPropUtil {
	
	private static MethodElementPropUtil methodElementPropUtil = new MethodElementPropUtil();
	public static MethodElementPropUtil getMethodElementPropUtil() {
		return methodElementPropUtil;
	}
	
	protected MethodElementPropUtil() {		
	}	
	
	protected String getStringValue(MethodElement element, String propName) {		
		MethodElementProperty prop = MethodElementPropertyHelper.getProperty(element, propName);
		if (prop == null) {
			return null;
		}
		return prop.getValue();
	}
	
	protected void setStringValue(MethodElement element, String propName, String value) {	
		MethodElementPropertyHelper.setProperty(element, propName, value);
	}	
	
	protected Boolean getBooleanValue(MethodElement element, String propName) {		
		MethodElementProperty prop = MethodElementPropertyHelper.getProperty(element, propName);
		if (prop == null) {
			return null;
		}
		String value = prop.getValue();
		return Boolean.parseBoolean(value);
	}
	
	protected void setBooleanValue(MethodElement element, String propName, boolean value) {	
		String strValue = value ? Boolean.TRUE.toString() : Boolean.FALSE.toString();
		MethodElementPropertyHelper.setProperty(element, propName, strValue);
	}
	
	protected Integer getIntValue(MethodElement element, String propName) {		
		MethodElementProperty prop = MethodElementPropertyHelper.getProperty(element, propName);
		if (prop == null) {
			return null;			
		}
		String value = prop.getValue();
		return Integer.parseInt(value);
	}
	
	protected void setIntValue(MethodElement element, String propName, int value) {	
		String strValue = Integer.toString(value);
		MethodElementPropertyHelper.setProperty(element, propName, strValue);
	}
	
}
