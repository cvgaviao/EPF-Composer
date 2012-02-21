package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedSection;
import org.eclipse.epf.uma.util.MetaElement;
import org.w3c.dom.Element;

public class MetaElementImpl implements MetaElement, IMetaDef, Adapter {

	private String id;
	private String name;
	private String globalId;
	private boolean suppressed = false;;
	private MetaElement parent;	
	private boolean inheritanceProcessed =false;
	private MetaElement superMeta;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public String getGlobalId() {
		if (getParent() != null) {
			return getParent().getGlobalId() + IMetaDef.scopeSeperator + getId();
		}
		return getId();
	}

	public MetaElement getParent() {
		return parent;
	}
	
	public void setParent(MetaElement parent) {
		this.parent = parent;
	}
	
	public boolean isSuppressed() {
		return suppressed;
	}
	
    public int compareTo(MetaElement o) {
    	return getName().compareTo(o.getName());
    }
	
	public void parseElement(Element element) throws TypeDefException {
		if (element == null) {
			return;
		}
		id = element.getAttribute(IMetaDef.ID);
		name = element.getAttribute(IMetaDef.NAME);
		String str = element.getAttribute(IMetaDef.SUPPRESSED);
		suppressed = str == null ? false : Boolean.parseBoolean(str.trim());
				
		globalId = id;
		Element nameElement = XMLUtil.getFirstChildElementByTagName(element, IMetaDef.NAME);
		if (nameElement != null) {
			name = nameElement.getTextContent();
		}
		name = name.trim();
	}
	
	//Return true if this method is called the first time
	public boolean processInheritance() {
		if (inheritanceProcessed) {
			return false;
		}
		inheritanceProcessed = true;
		return true;
	}
	
	protected static List<? extends MetaElement> processInherentList(List<? extends MetaElement> myList, List<? extends MetaElement> superList) {
		Map<String, MetaElement> map = new HashMap<String, MetaElement>();
		for (MetaElement meta : myList) {
			map.put(meta.getId(), meta);
		}	
		
		List<MetaElement> newList = new ArrayList<MetaElement>();
		for (MetaElement meta : superList) {
			MetaElement childMeta = map.remove(meta.getId());
			if (childMeta == null) {
				childMeta = meta;
			} else {
				childMeta.setSuperMeta(meta);
			}
			if (! childMeta.isSuppressed()) {
				newList.add(childMeta);
			}
		}	
		for (MetaElement meta : myList) {
			if (! meta.isSuppressed() && map.containsKey(meta.getId())) {
				newList.add(meta);
			}
		}
		
		return newList;
	}
	
	protected static List<? extends MetaElement> processSuppress(List<? extends MetaElement> myList) {
		List<MetaElement> newList = new ArrayList<MetaElement>();
		for (MetaElement meta : myList) {
			if (! meta.isSuppressed()) {
				newList.add(meta);
			}
		}		
		return newList;
	}
	
	//Adapter interface methods ->
	public void notifyChanged(Notification notification) {
	}

	public Notifier getTarget() {
		return null;
	}

	public void setTarget(Notifier newTarget) {
	}

	public boolean isAdapterForType(Object type) {
		return false;
	}
	
	public MetaElement getSuperMeta() {
		return superMeta;
	}

	public void setSuperMeta(MetaElement superMeta) {
		this.superMeta = superMeta;
	}
	//Adapter interface methods <-
	
}
