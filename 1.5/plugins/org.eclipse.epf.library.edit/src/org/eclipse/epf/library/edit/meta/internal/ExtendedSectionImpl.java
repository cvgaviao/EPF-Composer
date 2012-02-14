package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefUtil;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ExtendedSection;
import org.eclipse.epf.uma.util.MetaElement;
import org.eclipse.epf.uma.util.UmaUtil;
import org.w3c.dom.Element;

public class ExtendedSectionImpl  extends MetaElementImpl implements ExtendedSection {

	private String type;
	private List<ExtendedReference> references;
	private List<ExtendedAttribute> rtes;
	
	public ExtendedSectionImpl() {		
	}
	
	public String getType() {
		return type;
	}

	public List<ExtendedReference> getReferences() {
		if (references == null) {
			references = new ArrayList<ExtendedReference>();
		}
		return references;
	}
	
	public List<ExtendedAttribute> getRtes() {
		if (rtes == null) {
			rtes = new ArrayList<ExtendedAttribute>();
		}
		return rtes;
	}
	
	public void parseElement(Element element)	throws TypeDefException {
		super.parseElement(element);			
		type = element.getAttribute(IMetaDef.type);
		
		getReferences().clear();
		List<Element> referenceElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.REFERENCE);
		if (referenceElements != null) {
			for (Element rElement : referenceElements) {
				ExtendedReferenceImpl ref = new ExtendedReferenceImpl();
				ref.parseElement(rElement);
				references.add(ref);
			}
		}
		
		getRtes().clear();
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
