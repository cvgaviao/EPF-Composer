package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.w3c.dom.Element;

public class ModifiedTypeMetaImpl extends MetaElementImpl implements ModifiedTypeMeta {
		
	private List<ExtendedReference> references;
	private List<ExtendedAttribute> rtes;
	
	public ModifiedTypeMetaImpl() {		
	}
		
	public List<ExtendedReference> getReferences() {
		if (references == null) {
			return Collections.EMPTY_LIST;
		}
		return references;
	}
	
	public List<ExtendedAttribute> getRtes() {
		if (rtes == null) {
			return Collections.EMPTY_LIST;
		}
		return rtes;
	}
	
	public void parseElement(Element element)	throws TypeDefException {		
		super.parseElement(element);
		
		references = new ArrayList<ExtendedReference>();
		List<Element> referenceElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.REFERENCE);
		if (referenceElements != null) {
			for (Element rElement : referenceElements) {
				ExtendedReferenceImpl ref = new ExtendedReferenceImpl();
				ref.parseElement(rElement);
				references.add(ref);
			}
		}
		
		rtes = new ArrayList<ExtendedAttribute>();
		List<Element> rteElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.RTE);
		if (rtes != null) {
			for (Element rElement : rteElements) {
				ExtendedAttributeImpl rte = new ExtendedAttributeImpl();
				rte.parseElement(rElement);
				rtes.add(rte);
			}
		}
		
	}
	
}
