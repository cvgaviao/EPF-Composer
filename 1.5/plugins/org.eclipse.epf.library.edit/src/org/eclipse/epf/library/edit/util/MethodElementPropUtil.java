package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.command.MethodElementSetPropertyCommand;
import org.eclipse.epf.library.edit.uma.MethodElementExt;
import org.eclipse.epf.library.edit.uma.MethodElementExt.WorkProductStateExt;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;
import org.eclipse.epf.uma.util.UmaUtil;

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
	
	public void setStringValue(MethodElement element, String propName, String value) {	
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
	
	protected void removeProperty(MethodElement e, String propName) {
		MethodElementPropertyHelper.removeProperty(e, propName);
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
		if (isWorkProductState(element)) {
			return new WorkProductStateExt((Constraint) element);
		}
		return new MethodElementExt(element);
	}
	
	public boolean isWorkProductState(MethodElement element) {
		if (!(element instanceof Constraint)) {
			return false;
		}
		Constraint c = (Constraint) element;
		return ConstraintManager.Plugin_wpState.equals(c.getName());
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
	
	public List<? extends MethodElement> extractElements(MethodElement propertyOwner, String propName, String refName,
			Set<? extends MethodElement> validSet) {
		List<MethodElement> elements = new ArrayList<MethodElement>();
		
		String value = getStringValue(propertyOwner, propName);
		if (value == null || value.length() == 0) {
			return elements;
		}
		
		String[] infos = value.split(infoSeperator);
		if (infos == null || infos.length == 0) {
			return elements;
		}
		
		boolean modified = false;
		String newValue = ""; //$NON-NLS-1$
		int sz = infos.length / 2; 		
		for (int i = 0; i < sz; i++) {
			int i1 = i*2;
			int i2 = i1 + 1;
			String iGuid = infos[i1];
			String iRefName = infos[i2];			
			if (refName.equals(iRefName)) {
				MethodElement element = LibraryEditUtil.getInstance().getMethodElement(iGuid);
				if (element != null && validSet.contains(element)) {
					elements.add(element);
					if (newValue.length() > 0) {
						newValue = newValue.concat(infoSeperator);
					}
					newValue = newValue.concat(iGuid.concat(infoSeperator).concat(iRefName));
				} else {
					modified = true;
				}
			} else {
				if (newValue.length() > 0) {
					newValue = newValue.concat(infoSeperator);
				}
				newValue = newValue.concat(iGuid.concat(infoSeperator).concat(iRefName));
			}
		}		
		if (modified) {
			setStringValue(propertyOwner, propName, newValue);
		}
		
		return elements;
	}

	public boolean isTransientElement(MethodElement element) {
		MethodElementExt extObj = getExtendObject(element, false);
		return extObj == null ? false : extObj.isTransientElement();
	}

	public void setTransientElement(MethodElement element,
			boolean transientElement) {
		if (!transientElement && getExtendObject(element, false) == null) {
			return;
		}
		MethodElementExt extObj = getExtendObject(element, true);
		extObj.setTransientElement(transientElement);

	}
	
	//-> For work product state objects
	public void addToAssignedToWps(WorkProduct wp, Constraint state) {
		if (! isWorkProductState(state)) {
			return;
		}
		MethodElementExt extObj = getExtendObject(state, true);
		if (! (extObj instanceof WorkProductStateExt)) {
			return;
		}
		((WorkProductStateExt) extObj).addToAssignedToWps(wp);
	}
	
	public void removeFromAssignedToWps(WorkProduct wp, Constraint state) {
		MethodElementExt extObj = getExtendObject(state, false);
		if (! (extObj instanceof WorkProductStateExt)) {
			return;
		}
		((WorkProductStateExt) extObj).removeFromAssignedToWps(wp);
	}
	
	public List<WorkProduct> getAssignedToWorkProducts(Constraint state) {
		MethodPlugin plugin = UmaUtil.getMethodPlugin(state);
		if (plugin != null) {
			MethodPluginPropUtil.getMethodPluginPropUtil().loadWpStates(plugin);
		}	
		
		MethodElementExt extObj = getExtendObject(state, false);
		if (! (extObj instanceof WorkProductStateExt)) {
			return Collections.EMPTY_LIST;
		}
		return ((WorkProductStateExt) extObj).getAssignedToWorkProducts();
	}
	//<- For work product state objects
}
