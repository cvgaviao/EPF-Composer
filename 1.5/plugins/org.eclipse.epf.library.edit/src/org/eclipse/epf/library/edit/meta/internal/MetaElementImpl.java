package org.eclipse.epf.library.edit.meta.internal;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.MetaElement;
import org.w3c.dom.Element;

public class MetaElementImpl implements MetaElement, IMetaDef, Adapter {

	private String id;
	private String name;
	private String globalId;
	private MetaElement parent;

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
	
    public int compareTo(MetaElement o) {
    	return getName().compareTo(o.getName());
    }
	
	public void parseElement(Element element) throws TypeDefException {
		if (element == null) {
			return;
		}
		id = element.getAttribute(IMetaDef.ID);
		name = element.getAttribute(IMetaDef.NAME);
		globalId = id;
		Element nameElement = XMLUtil.getFirstChildElementByTagName(element, IMetaDef.NAME);
		if (nameElement != null) {
			name = nameElement.getTextContent();
		}
		name = name.trim();
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
	//Adapter interface methods <-
	
}
