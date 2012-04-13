package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ExtendedFeature;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ExtendedSection;
import org.eclipse.epf.uma.util.ExtendedTable;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.w3c.dom.Element;

public class ModifiedTypeMetaImpl extends MetaElementImpl implements ModifiedTypeMeta {
		
	private List<ExtendedReference> references;
	private List<ExtendedAttribute> rtes;

	private List<ExtendedSection> sections;
	private List<ExtendedSection> referenceSections;
	private List<ExtendedSection> rteSections;
	private List<ExtendedTable> tables;
	
	private List<String> linkTypes;

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
	
	public List<ExtendedTable> getTables() {
		if (tables == null) {
			tables = new ArrayList<ExtendedTable>();
		}
		return tables;
	}
		
	public List<String> getLinkTypes() {
		if (linkTypes == null) {
			linkTypes = new ArrayList<String>();
		}
		return linkTypes;
	}
	
	public void parseElement(Element element)	throws TypeDefException {		
		super.parseElement(element);
				
		getSections().clear();
		getLinkTypes().clear();
		
		List<Element> linkTypeElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.linkType);
		if (linkTypeElements != null) {
			for (Element linkTypeElement : linkTypeElements) {
				String linkType = linkTypeElement.getAttribute(IMetaDef.ID);
				getLinkTypes().add(linkType);
			}
		}
		
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
		
		return true;
	}

	private void init() {
		getReferences().clear();
		getRtes().clear();		
		getReferenceSections().clear();
		getRteSections().clear();
		getTables().clear();

		for (ExtendedSection section : getSections()) {
			if (IMetaDef.REFERENCE.equals(section.getType())) {
				getReferenceSections().add(section);
				getReferences().addAll(section.getReferences());
				getTables().addAll(section.getTables());

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
		if (extendedFeatures == null) {
			extendedFeatures = new HashSet<ExtendedFeature>();
		}
		extendedFeatures.addAll(linkedMeta.getReferences());
		extendedFeatures.addAll(linkedMeta.getRtes());
		return true;
	}
	
	private Set<ExtendedFeature> extendedFeatures;
	public boolean isLinkedFeature(ExtendedFeature feature) {
		return extendedFeatures == null ? false : extendedFeatures.contains(feature);
	}
	
}
