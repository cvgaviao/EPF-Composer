package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.command.MethodElementSetPropertyCommand;
import org.eclipse.epf.library.edit.uma.MethodElementExt;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;

public class MethodElementPropUtil {
	
	public static final String infoSeperator = "/"; 							//$NON-NLS-1$
	
	public static final String CONSTRAINT_WPStates = "constraint_wpStates";		//$NON-NLS-1$

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
		
	public MethodElementExt getExtendObject(MethodElement element, boolean create) {
		MultiResourceEObject mobj = (MultiResourceEObject) element;		
		ExtendObject obj = mobj.getExtendObject();
		if (create && !(obj instanceof MethodElementExt)) {
			obj = createExtendObject(element);
			mobj.setExtendObject(obj);
		}
		return (MethodElementExt) obj;
	}
	
	protected MethodElementExt createExtendObject(MethodElement element) {
		return new MethodElementExt(element);
	}
	
	public void addReferenceInfo(MethodElement owner, MethodElement reference, String propName, String refName) {
		String oldValue = getStringValue(owner, propName);
		String newValue = reference.getGuid().concat(infoSeperator).concat(refName);

		if (oldValue != null && oldValue.length() > 0) {			
			String[] infos = oldValue.split(infoSeperator); 
			
			int sz = infos.length / 2; 		
			for (int i = 0; i < sz; i++) {
				int i1 = i*2;
				int i2 = i1 + 1;
				String iGuid = infos[i1];
				String iFeature = infos[i2];
				if (iGuid.equals(reference.getGuid()) && iFeature.equals(refName)) {
					return;
				} 
			}
			
			newValue = oldValue.concat(infoSeperator).concat(newValue);
		}				
		setStringValue(owner, propName, newValue);
	}
	
	public void removeReferenceInfo(MethodElement owner, MethodElement reference, String propName, String refName) {
		String oldValue = getStringValue(owner, propName);
		if (oldValue == null || oldValue.length() == 0) {
			return;
		}
		boolean removed = false;
		String newValue = ""; //$NON-NLS-1$

		if (oldValue != null && oldValue.length() > 0) {			
			String[] infos = oldValue.split(infoSeperator); 
			
			int sz = infos.length / 2; 		
			for (int i = 0; i < sz; i++) {
				int i1 = i*2;
				int i2 = i1 + 1;
				String iGuid = infos[i1];
				String iFeature = infos[i2];
				if (iGuid.equals(reference.getGuid()) && iFeature.equals(refName)) {
					removed = true;		
				} else {
					if (newValue.length() > 0) {
						newValue = newValue.concat(infoSeperator);
					}
					newValue = newValue.concat(iGuid.concat(infoSeperator).concat(iFeature));
				}
			}

		}
		
		if (removed) {
			setStringValue(owner, propName, newValue);
		}

	}
	
	public List<? extends MethodElement> extractElements(MethodElement propertyOwner, String propName, String refName) {
		List<MethodElement> elements = new ArrayList<MethodElement>();
		
		String value = getStringValue(propertyOwner, propName);
		if (value == null || value.length() == 0) {
			return elements;
		}
		
		String[] infos = value.split(infoSeperator);
		if (infos == null || infos.length == 0) {
			return elements;
		}
		

		int sz = infos.length / 2; 		
		for (int i = 0; i < sz; i++) {
			int i1 = i*2;
			int i2 = i1 + 1;
			String iGuid = infos[i1];
			String iRefName = infos[i2];			
			if (refName.equals(iRefName)) {
				MethodElement element = LibraryEditUtil.getInstance().getMethodElement(iGuid);
				if (element != null) {
					elements.add(element);
				}
			} 
		}		

		return elements;
	}

}
