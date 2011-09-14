package org.eclipse.epf.library.edit.uma;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.library.edit.util.XmlEditUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.MeList;
import org.eclipse.epf.uma.util.UmaUtil;
import org.w3c.dom.Element;

public class ExtendReferenceMap {

	//Reference names
	public static final String UtdList = "udtList";			//$NON-NLS-1$
	
	public static String[] names = {
		UtdList,
	};
	
	private Map<String, Object> map;
	private Map<String, Object> oldValueMap;
	private MethodElement ownerElement;
	
	public ExtendReferenceMap(MethodElement ownerElement) {
		this.ownerElement = ownerElement;
	}
	
	public MethodElement getOwnerElement() {
		return ownerElement;
	}
	
	public void retrieveReferencesFromElement(Element element) {
		for (String name : names) {
			String value = element.getAttribute(name);
			if (value == null || value.length() == 0) {
				continue;
			}
			UnresolvedGuidHandler uHandler = new UnresolvedGuidHandler();
			MeList items = XmlEditUtil.convertToMethodElements(value, UmaPackage.eINSTANCE.getPractice(), uHandler);
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
		if (! isMany(name)) {
			return value;
		}
		
		if (value == null && toModify) {
			value = new MeList();
			getMap().put(name, value);
		}			
		if (! (value instanceof MeList)) {
			return value;
		}
		
		MeList meList = (MeList) value;
		if (!meList.isOFeatureHandled()) {
			for (Object obj : meList) {
				if (obj instanceof MethodElement) {
					MethodElement element = (MethodElement) obj; 
					addOpposite(name, element);
				}
			}					
			meList.setOFeatureHandled(true);
		}
		if (meList.isHasUnresolved()) {
			boolean allResoved = true;					
			for (int i = 0; i < meList.size(); i++) {
				Object obj = meList.get(i);
				if (obj instanceof MethodElement) {
					MethodElement element = (MethodElement) obj;
					if (UmaUtil.isUnresolved(element)) {
						MethodElement resolveElement = LibraryEditUtil.getInstance().getMethodElement(element.getGuid());
						if (resolveElement == null) {
							allResoved = false;
						} else {
							meList.set(i, resolveElement);
							addOpposite(name, resolveElement);
						}
					}
				}
			}					
			if (allResoved) {
				meList.setHasUnresolved(false);
			}
		}
		if (toModify && !getOldValueMap().containsKey(name)) {
			Object oldValue = meList.clone();
			getOldValueMap().put(name, oldValue);
		}

		return meList;

	}

	public void addOpposite(String name, MethodElement element) {
		if (UmaUtil.isUnresolved(element)) {
			return;
		}
		MethodElementPropUtil propUtil = MethodElementPropUtil.getMethodElementPropUtil();
		ExtendReferenceMap otherMap = propUtil.getExtendReferenceMap(element, true);
		Object ovalue = otherMap.get(getOppositeName(name), true);
		if (ovalue instanceof MeList) {
			((MeList) ovalue).add(getOwnerElement());
		}
	}
	
	public void removeOpposite(String name, MethodElement element) {
		if (UmaUtil.isUnresolved(element)) {
			return;
		}
		MethodElementPropUtil propUtil = MethodElementPropUtil.getMethodElementPropUtil();
		ExtendReferenceMap otherMap = propUtil.getExtendReferenceMap(element, true);
		Object ovalue = otherMap.get(getOppositeName(name), true);
		if (ovalue instanceof MeList) {
			((MeList) ovalue).remove(getOwnerElement());
		}
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
	
	private static class UnresolvedGuidHandler extends XmlEditUtil.UnresolvedGuidHandler {
		
		@Override
		public MethodElement getElement(String guid) {
			Practice practic = UmaFactory.eINSTANCE.createPractice();
			practic.setGuid(guid);
			UmaUtil.setUnresolved(practic);					
			return practic;
		}
		
		@Override
		public boolean hasUnresolvedElement() {
			return false;
		}
		
		
		
	};
	
}
