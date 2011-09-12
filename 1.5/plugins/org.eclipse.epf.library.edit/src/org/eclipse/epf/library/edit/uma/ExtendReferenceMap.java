package org.eclipse.epf.library.edit.uma;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.library.edit.util.XmlEditUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.UmaPackage;
import org.w3c.dom.Element;

public class ExtendReferenceMap {

	//Reference names
	public static final String UtdList = "udtList";			//$NON-NLS-1$
	
	public static String[] names = {
		UtdList,
	};
	
	private Map<String, Object> map;
	private Map<String, Object> oldValueMap;

	public ExtendReferenceMap() {		
	}
	
	public void retrieveReferencesFromElement(Element element) {
		for (String name : names) {
			String value = element.getAttribute(name);
			if (value == null || value.length() == 0) {
				continue;
			}			
			List<Practice> items = (List<Practice>) XmlEditUtil.convertToMethodElements(value, UmaPackage.eINSTANCE.getPractice());
			if (items != null && !items.isEmpty()) {
				getMap().put(name, items);
			}
		}
	}
	
	public Object get(String name, boolean toModify) {
		if (!toModify && map == null) {
			return null;
		}
		Object value = getMap().get(name);
		if (isMany(name) && toModify) {
			if (value == null) {
				value = new MeList();
				getMap().put(name, value);
			}
			if (value instanceof MeList) {
				Object oldValue = ((MeList) value).clone();
				getOldValueMap().put(name, oldValue);
			}
		}
		return value;
	}

	public void set(String name, Object value) {
		Object oldValue = get(name, false);
		if (oldValue instanceof MeList) {
			oldValue = ((MeList) oldValue).clone();
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
	
	public void storeReferencesToElement(Element element) {
		storeReferencesToElement(element, false);
		getOldValueMap().clear();
	}
	
	public void storeReferencesToElement(Element element, boolean rollback) {
		for (String name : names) {
			Object value = get(name, false);
			if (rollback && getOldValueMap().containsKey(name)) {
				value = getOldValueMap().get(name);
				if (value == null) {
					getMap().remove(name);
				} else {
					getMap().put(name, value);
				}
			}
			if (value instanceof MethodElement) {
				MethodElement eValue = (MethodElement) value;
				element.setAttribute(name, eValue.getGuid());

			} else if (value instanceof List) {
				String str = ""; //$NON-NLS-1$	
				for (Object item : (List) value) {
					if (item instanceof MethodElement) {
						MethodElement eValue = (MethodElement) item;
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
	
	private static class MeList extends ArrayList {
		
		private boolean hasUnresolved = false;

		public boolean isHasUnresolved() {
			return hasUnresolved;
		}

		public void setHasUnresolved(boolean hasUnresolved) {
			this.hasUnresolved = hasUnresolved;
		} 
		
	}
	
	
	
}
