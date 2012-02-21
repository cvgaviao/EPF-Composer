package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefUtil;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.QualifiedReference;
import org.eclipse.epf.uma.util.UmaUtil;
import org.w3c.dom.Element;

public class ExtendedReferenceImpl extends MetaElementImpl implements ExtendedReference {
	private EReference ref;	

	private List<QualifiedReference> qualifiedReferences;

	public ExtendedReferenceImpl() {		
	}
	
	public EReference getReference() {
		return ref;
	}
	
	protected void setReference(EReference ref) {
		this.ref = ref;
	}
	
	public List<QualifiedReference> getQualifiedReferences() {
		return qualifiedReferences;
	}
	
	public void parseElement(Element element)	throws TypeDefException {
		super.parseElement(element);
		if (element == null) {
			return;
		}
		ref =  UmaUtil.createReference(getId());
		TypeDefUtil.getInstance().associate(this, ref);
		
		qualifiedReferences = new ArrayList<QualifiedReference>();
		List<Element> rqElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.REFERENCE_QUALIFIERS);
		if (rqElements == null || rqElements.isEmpty()) {
			return;
		}
		for (Element rqElement : rqElements) {
			List<Element> qElements = XMLUtil.getChildElementsByTagName(rqElement, IMetaDef.QUALIFIER);
			for (Element qElement : qElements) {
				QualifiedReferenceImpl q = new QualifiedReferenceImpl();
				q.setParent(this);
				q.parseElement(qElement);
				qualifiedReferences.add(q);
			}
		}
	}
	
	@Override
	public boolean processInheritance() {
		if (! super.processInheritance()) {
			return false;
		}		
		
		if (getSuperMeta() == null) {
			qualifiedReferences = (List<QualifiedReference>) processSuppress(this.getQualifiedReferences());
			return true;
			
		} 
		
		if (getSuperMeta() instanceof ExtendedReferenceImpl) {			
			ExtendedReferenceImpl sMeta = (ExtendedReferenceImpl) getSuperMeta();
			sMeta.processInheritance();			
			qualifiedReferences = (List<QualifiedReference>) processInherentList(this.getQualifiedReferences(), sMeta.getQualifiedReferences());
			for (QualifiedReference qref : getQualifiedReferences()) {
				qref.processInheritance();
			}
			return true;
			
		}
		return false;
	}
}
