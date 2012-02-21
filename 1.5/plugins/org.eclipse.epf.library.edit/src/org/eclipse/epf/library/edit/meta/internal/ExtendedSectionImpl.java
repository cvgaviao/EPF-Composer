package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ExtendedSection;
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

	@Override
	public boolean processInheritance() {
		if (! super.processInheritance()) {
			return false;
		}
		
		if (getSuperMeta() == null) {
			references = (List<ExtendedReference>) processSuppress(this.getReferences());
			rtes = (List<ExtendedAttribute>) processSuppress(this.getRtes());
			return true;

		} 
		
		if (getSuperMeta() instanceof ExtendedSectionImpl){
			ExtendedSectionImpl sMeta = (ExtendedSectionImpl) getSuperMeta();
			sMeta.processInheritance();
			
			references = (List<ExtendedReference>) processInherentList(this.getReferences(), sMeta.getReferences());
			for (ExtendedReference ref : getReferences()) {
				ref.processInheritance();
			}
			
			rtes = (List<ExtendedAttribute>) processInherentList(this.getRtes(), sMeta.getRtes());
			for (ExtendedAttribute att : getRtes()) {
				att.processInheritance();
			}
			return true;
		} 
		
		return false;
	}
	
}
