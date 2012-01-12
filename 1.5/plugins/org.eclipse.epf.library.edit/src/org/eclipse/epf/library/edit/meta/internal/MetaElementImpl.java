package org.eclipse.epf.library.edit.meta.internal;

import org.eclipse.epf.library.edit.meta.ITypeDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.MetaElement;
import org.w3c.dom.Element;

public class MetaElementImpl implements MetaElement, ITypeDef {

	private String id;
	private String name;
	
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

	public ITypeDef parse(Element element)	throws TypeDefException {
		return null;
	}
	
}
