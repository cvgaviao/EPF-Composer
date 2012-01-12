package org.eclipse.epf.library.edit.meta.internal;

import java.util.List;

import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.w3c.dom.Element;

public class ModifiedTypeMetaImpl extends MetaElementImpl implements ModifiedTypeMeta {
		
	private List<ExtendedReference> references;

	public ModifiedTypeMetaImpl() {		
	}
		
	public List<ExtendedReference> getReferences() {
		return references;
	}
	
	public IMetaDef parseElement(Element element)	throws TypeDefException {
		return null;
	}
	
}
