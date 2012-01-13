package org.eclipse.epf.library.edit.meta.internal;

import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.w3c.dom.Element;

public class QualifiedReferenceImpl extends ExtendedReferenceImpl {

	public QualifiedReferenceImpl() {		
	}
	
	public void parseElement(Element element)	throws TypeDefException {
		super.parseElement(element);		
		String name = element.getTextContent();
		setName(name);
		
		String scopeId = getId();
		ExtendedReference parent = getNestedParent();
		if (parent != null) {
			scopeId = parent.getId() + IMetaDef.scopeSeperator + scopeId;
			parent = parent.getNestedParent();
		}
	}
	
}
