package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.command.MethodElementSetPropertyCommand;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;

public class MethodElementPropUtil {
	
	private static MethodElementPropUtil methodElementPropUtil = new MethodElementPropUtil();
	public static MethodElementPropUtil getMethodElementPropUtil() {
		return methodElementPropUtil;
	}
	
	private IActionManager actionManager;
	
	protected MethodElementPropUtil() {		
	}
	
	protected MethodElementPropUtil(IActionManager actionManager) {
		this.actionManager = actionManager;
	}	
	
	public IActionManager getActionManager() {
		return actionManager;
	}
	
	protected String getStringValue(MethodElement element, String propName) {		
		MethodElementProperty prop = MethodElementPropertyHelper.getProperty(element, propName);
		if (prop == null) {
			return null;
		}
		return prop.getValue();
	}
	
	protected void setStringValue(MethodElement element, String propName, String value) {	
		setProperty(element, propName, value);
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
		setProperty(element, propName, strValue);
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
		setProperty(element, propName, strValue);
	}
	
	private void setProperty(MethodElement e, String propName, String propValue) {
		if (getActionManager() == null) {
			MethodElementPropertyHelper.setProperty(e, propName, propValue);
		} else {
			MethodElementSetPropertyCommand cmd = new MethodElementSetPropertyCommand(
					e, propName, propValue);
			getActionManager().execute(cmd);
		}
	}
	
	protected Object getValitileObject(MethodElement element, String key) {
		MultiResourceEObject mobj = (MultiResourceEObject) element;
		return mobj.getVolatileObject(key);
	}
	
	protected void storeValitileObject(MethodElement element, String key, Object value) {
		MultiResourceEObject mobj = (MultiResourceEObject) element;
		mobj.storeVolatileObject(key, value);
	}
	
}
