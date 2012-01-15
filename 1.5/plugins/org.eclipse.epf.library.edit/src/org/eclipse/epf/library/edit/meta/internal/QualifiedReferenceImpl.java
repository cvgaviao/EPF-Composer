package org.eclipse.epf.library.edit.meta.internal;

import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.uma.ExtendReferenceMap;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.QualifiedReference;
import org.w3c.dom.Element;

public class QualifiedReferenceImpl extends ExtendedReferenceImpl implements QualifiedReference {
	
	/**
	 * Validation note:
	 * (1) Uniqueness of names and id
	 * (2) Reserved key words
	 * (3) Forbidden chars
	 */
	public QualifiedReferenceImpl() {		
	}
	
	public void parseElement(Element element)	throws TypeDefException {
		super.parseElement(element);
		if (element == null) {
			return;
		}
		String name = element.getTextContent();
		setName(name);
		
		String globalId = getId();
		ExtendedReference parent = getNestedParent();
		if (parent != null) {
			globalId = parent.getId() + IMetaDef.scopeSeperator + globalId;
			parent = parent.getNestedParent();
		}
		globalId = ExtendReferenceMap.MdtQReference_ + globalId;
		setGlobalId(globalId);		
	}
	
}
