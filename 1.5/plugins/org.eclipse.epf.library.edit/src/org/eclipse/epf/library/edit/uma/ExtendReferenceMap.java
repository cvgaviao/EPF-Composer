package org.eclipse.epf.library.edit.uma;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.uma.MethodElement;
import org.w3c.dom.Element;

public class ExtendReferenceMap {

	//Reference names
	public static final String PracticeUtd = "practiceUtd";			//$NON-NLS-1$
	
	public static String[] names = {
		PracticeUtd,
	};
	
	private Map<String, Object> map;
	private Map<String, Object> oldValueMap;

	public ExtendReferenceMap() {		
	}
	
	public Object get(String name, boolean toModify) {
		if (!toModify && map == null) {
			return null;
		}
		Object value = getMap().get(name);
		if (isMany(name) && toModify) {
			if (value == null) {
				value = new ArrayList<Object>();
				getMap().put(name, value);
			}
			if (value instanceof ArrayList) {
				value = ((ArrayList) value).clone();
				getOldValueMap().put(name, value);
			}
		}
		return value;
	}

	public void set(String name, Object value) {
		Object oldValue = get(name, false);
		if (oldValue instanceof ArrayList) {
			oldValue = ((ArrayList) oldValue).clone();
		}
		getOldValueMap().put(name, oldValue);
		getMap().put(name, value);
	}
	
	private Map<String, Object> getMap() {
		if (map == null) {
			map = new HashMap<String, Object>();
		}
		return map;
	}
	
	
	private Map<String, Object> getOldValueMap() {
		if (oldValueMap == null) {
			oldValueMap = new HashMap<String, Object>();
		}
		return oldValueMap;
	}
	
	public void storeReferences(Element element, boolean rollback) {
		for (String name : names) {
			Object value = get(name, false);
			if (rollback && getOldValueMap().containsKey(name)) {
				value = getOldValueMap().get(name);
			}
			if (value instanceof MethodElement) {
				MethodElement eValue = (MethodElement) value;
				element.setAttribute(name, eValue.getGuid());

			} else if (value instanceof List) {
				String str = ""; //$NON-NLS-1$	
				for (Object item : (List) value) {
					if (value instanceof MethodElement) {
						MethodElement eValue = (MethodElement) value;
						if (str.length() > 0) {
							str += MethodElementPropUtil.infoSeperator;
						}
						str += eValue.getGuid();
					}
				}
				element.setAttribute(name, str);
			}
		}
		if (rollback) {
			getOldValueMap().clear();
		}
	}

	public static String getOppositeName(String name) {
		return "opposite_" + name;									//$NON-NLS-1$
	}

	public boolean isMany(String name) {
		return true;
	}
	
}
