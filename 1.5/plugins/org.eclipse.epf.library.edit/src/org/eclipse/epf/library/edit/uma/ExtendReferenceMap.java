package org.eclipse.epf.library.edit.uma;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.library.edit.util.PracticePropUtil;
import org.eclipse.epf.library.edit.util.PropUtil;
import org.eclipse.epf.library.edit.util.XmlEditUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.MeList;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.eclipse.epf.uma.util.QualifiedReferences;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.epf.uma.util.UserDefinedTypeMeta;
import org.w3c.dom.Element;

public class ExtendReferenceMap {

	//Reference names
	public static final String UtdList = "udtList";					//$NON-NLS-1$
	
	private static final String Opposite_ = "opposite_";			//$NON-NLS-1$
	private static final String QReference_ = "qReference_";		//$NON-NLS-1$
	private static final String MdtQReference_ = "mdtQReference_";		//$NON-NLS-1$
	public static final String WSpace = "__ws__";					//$NON-NLS-1$
	
	private Map<String, Object> map;
	private Map<String, Object> oldValueMap;
	private MethodElement ownerElement;
	private List<String> referenceNames;
	private boolean retrieved = false;

	public ExtendReferenceMap(MethodElement ownerElement) {
		this.ownerElement = ownerElement;
		referenceNames = new ArrayList<String>();
		referenceNames.add(UtdList);
		List<String> refQualifieIds = getReferenceQualifierIds(ownerElement);
		if (refQualifieIds != null && ! refQualifieIds.isEmpty()) {
			for (String qualifierId : refQualifieIds) {
				String name = getQReferenceNameById(qualifierId);
				referenceNames.add(name);
			}
		}
		
		refQualifieIds = getReferenceMdtQualifierIds(ownerElement);
		if (refQualifieIds != null && ! refQualifieIds.isEmpty()) {
			for (String qualifierId : refQualifieIds) {
				String name = getMdtQReferenceNameById(qualifierId);
				referenceNames.add(name);
			}
		}
	}
			
	public boolean isRetrieved() {
		return retrieved;
	}
	
	private List<String> getReferenceQualifierIds(MethodElement element) {
		if (! (element instanceof Practice)) {
			return null;
		}		
		List<String> refQualifieIds = new ArrayList<String>();		
		PracticePropUtil propUtil = PracticePropUtil.getPracticePropUtil();
		UserDefinedTypeMeta meta = propUtil.getUdtMeta((Practice) element);
		if (meta != null) {
			for (EReference ref : meta.getQualifiedReferences()) {
				refQualifieIds.add(ref.getName());
			}		
		}
		return refQualifieIds;
	}
	
	private List<String> getReferenceMdtQualifierIds(MethodElement element) {	
		PropUtil propUtil = PropUtil.getPropUtil();
		ModifiedTypeMeta meta = propUtil.getMdtMeta(element);
		if (meta == null) {
			return null;
		}
		
		List<String> refQualifieIds = new ArrayList<String>();	
		for (ExtendedReference extendRef : meta.getReferences()) {
			for (EReference ref : extendRef.getQualifiedReferences()) {
				refQualifieIds.add(ref.getName());
			}
		}
		return refQualifieIds;
	}
	
	public MethodElement getOwnerElement() {
		return ownerElement;
	}
	
	public void notifyOwnerElementSaved() {
		getOldValueMap().clear();
	}
	
	public void retrieveReferencesFromElement(Element element) {
		retrieved = true;
		
		Set<MethodElement> referenceSet = null;
		if (ownerElement instanceof Practice) {
			PracticePropUtil propUtil = PracticePropUtil.getPracticePropUtil();
			Practice practice = (Practice) ownerElement;
			if (propUtil.isUdtType(practice)) {
				referenceSet = new HashSet<MethodElement>();
				referenceSet.addAll(practice.getActivityReferences());
				referenceSet.addAll(practice.getContentReferences());
			}
		}
		
		Map<String, Set<MethodElement>> mdtQrValidSetMap = new HashMap<String, Set<MethodElement>>();
		for (String name : referenceNames) {
			String value = element.getAttribute(name);
			if (value == null || value.length() == 0) {
				continue;
			}
			UnresolvedGuidHandler uHandler = new UnresolvedGuidHandler();
			Set<MethodElement> validSet = null;
			if (name.startsWith(MdtQReference_)) {
				int i0 = MdtQReference_.length();
				int i1 = name.indexOf(QualifiedReferences.scopeSeperator);
				if (i1 > i0) {
					String parentRefName = name.substring(i0, i1);
					validSet = getMdtQrValidSet(element, mdtQrValidSetMap,
							uHandler, parentRefName);
				}
			} else if (referenceSet != null && name.startsWith(QReference_)) {
				validSet = referenceSet;
			}
			MeList items = XmlEditUtil.convertToMethodElements(value, getRetrieveType(name), uHandler, validSet);
			
			if (items != null && !items.isEmpty()) {
				getMap().put(name, items);
			}
		}
	}

	private Set<MethodElement> getMdtQrValidSet(Element element,
			Map<String, Set<MethodElement>> mdtQrValidSetMap,
			UnresolvedGuidHandler uHandler, String parentRefName) {
		Set<MethodElement> validSet;
		validSet = mdtQrValidSetMap.get(parentRefName);
		if (validSet == null) {
			validSet = new HashSet<MethodElement>();
			String parentRefValue = element.getAttribute(parentRefName);
			MeList items = XmlEditUtil.convertToMethodElements(parentRefValue, getRetrieveType(parentRefName), uHandler, null);
			validSet.addAll(items);
			mdtQrValidSetMap.put(parentRefName, validSet);
		}
		return validSet;
	}
	
	private EClass getRetrieveType(String referenceName) {
		if (referenceName.equals(UtdList)) {
			return UmaPackage.eINSTANCE.getPractice();
		}
		return  null;
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
		for (String name : referenceNames) {
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
		return Opposite_ + name;									
	}
	
	public static String getQReferenceNameById(String qualifiedId) {
		return getQReferenceNameById(qualifiedId, QReference_);
	}
	
	public static String getMdtQReferenceNameById(String qualifiedId) {
		return getQReferenceNameById(qualifiedId, MdtQReference_);
	}
	
	private static String getQReferenceNameById(String qualifiedId, String prefix) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < qualifiedId.length(); i++) {
			char c = qualifiedId.charAt(i);
			if (c == ' ' || c == '\t' || c == '\n') {
				sb.append(WSpace);
			} else {
				sb.append(c);
			}
		}
		if (sb.length() != qualifiedId.length()) {
			qualifiedId = sb.toString();
		}
		return prefix + qualifiedId;
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
