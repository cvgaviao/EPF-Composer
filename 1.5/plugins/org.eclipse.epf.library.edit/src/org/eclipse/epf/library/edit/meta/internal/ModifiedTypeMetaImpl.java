package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ExtendedSection;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.w3c.dom.Element;

public class ModifiedTypeMetaImpl extends MetaElementImpl implements ModifiedTypeMeta {
		
	private List<ExtendedReference> references;
	private List<ExtendedAttribute> rtes;
	List<ExtendedSection> sections;
	List<ExtendedSection> referenceSections;
	List<ExtendedSection> rteSections;
	
	public ModifiedTypeMetaImpl() {		
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
	
	public List<ExtendedSection> getSections() {
		if (sections == null) {
			sections = new ArrayList<ExtendedSection>();
		}
		return sections;
	}
	
	public List<ExtendedSection> getReferenceSections() {
		if (referenceSections == null) {
			referenceSections = new ArrayList<ExtendedSection>();
		}
		return referenceSections;
	}
	
	public List<ExtendedSection> getRteSections() {
		if (rteSections == null) {
			rteSections = new ArrayList<ExtendedSection>();
		}
		return rteSections;
	}
	
	public void parseElement(Element element)	throws TypeDefException {		
		super.parseElement(element);
		
		getReferences().clear();
		getRtes().clear();		
		getSections().clear();
		getReferenceSections().clear();
		getRteSections().clear();
		
		List<Element> sectionElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.SECTION);
		if (sectionElements != null) {
			for (Element sElement : sectionElements) {
				ExtendedSectionImpl ses = new ExtendedSectionImpl();
				ses.parseElement(sElement);
				
				getSections().add(ses);
				if (IMetaDef.REFERENCE.equals(ses.getType())) {
					getReferenceSections().add(ses);
					getReferences().addAll(ses.getReferences());
					
				} else if (IMetaDef.RTE.equals(ses.getType())) {
					getRteSections().add(ses);
					getRtes().addAll(ses.getRtes());
					
				}
			}
		}
	}
	
}
