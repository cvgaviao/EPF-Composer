package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ExtendedSection;
import org.eclipse.epf.uma.util.MetaElement;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.w3c.dom.Element;

public class ModifiedTypeMetaImpl extends MetaElementImpl implements ModifiedTypeMeta {
		
	private List<ExtendedReference> references;
	private List<ExtendedAttribute> rtes;

	List<ExtendedSection> sections;
	List<ExtendedSection> referenceSections;
	List<ExtendedSection> rteSections;
	
	public ModifiedTypeMetaImpl() {
		super(null);
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
	
	@Override
	public boolean isSuppressed() {
		return false;
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
				ExtendedSectionImpl ses = new ExtendedSectionImpl(this);
				ses.parseElement(sElement);				
				getSections().add(ses);
			}
		}
		init();
	}
	
	@Override
	public boolean processInheritance() {
		if (! super.processInheritance()) {
			return false;
		}
		
		if (getSuperMeta() == null) {
			int sz = getSections().size();
			sections = (List<ExtendedSection>) processSuppress(this.getSections());
			if (sz != sections.size()) {
				init();
			}
			return true;
		} 
		
		if (getSuperMeta() instanceof ModifiedTypeMetaImpl){
			ModifiedTypeMetaImpl sMeta = (ModifiedTypeMetaImpl) getSuperMeta();
			sMeta.processInheritance();			
			sections = (List<ExtendedSection>) processInherentList(this.getSections(), sMeta.getSections());
			for (ExtendedSection section : getSections()) {
				section.processInheritance();
			}
			init();
			return true;
		}
		
		return false;
	}

	private void init() {
		getReferences().clear();
		getRtes().clear();		
		getReferenceSections().clear();
		getRteSections().clear();

		for (ExtendedSection section : getSections()) {
			if (IMetaDef.REFERENCE.equals(section.getType())) {
				getReferenceSections().add(section);
				getReferences().addAll(section.getReferences());

			} else if (IMetaDef.RTE.equals(section.getType())) {
				getRteSections().add(section);
				getRtes().addAll(section.getRtes());
			}
		}
	}
	
	public boolean processLink(ModifiedTypeMeta linkedMeta) {
		if (linkedMeta == null) {
			return false;
		}
		getSections().addAll(linkedMeta.getSections());
		getReferences().addAll(linkedMeta.getReferences());
		getRtes().addAll(linkedMeta.getRtes());
		getReferenceSections().addAll(linkedMeta.getReferenceSections());;
		getRteSections().addAll(linkedMeta.getRteSections());
		return true;
	}
	
}
