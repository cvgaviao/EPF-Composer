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
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.QualifiedReference;
import org.eclipse.epf.uma.util.UmaUtil;
import org.w3c.dom.Element;

public class ExtendedReferenceImpl extends MetaElementImpl implements ExtendedReference, Adapter {
	private EReference ref;	
	private ExtendedReference nestedParent;		//for future nested extended reference structure

	private List<QualifiedReference> qualifiedReferences;

	public ExtendedReferenceImpl() {		
	}
	
	protected ExtendedReferenceImpl(ExtendedReference netedParent) {
		this.nestedParent = netedParent;
	}
	
	public ExtendedReference getNestedParent() {
		return nestedParent;
	}
	
	public void setNestedParent(ExtendedReference nestedParent) {
		this.nestedParent = nestedParent;
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
		ref =  UmaUtil.createReference(getId());
		ref.eAdapters().add(this);
		
		qualifiedReferences = new ArrayList<QualifiedReference>();
		List<Element> qElements = XMLUtil.getChildElementsByTagName(element, IMetaDef.QUALIFIER);
		if (qElements == null || qElements.isEmpty()) {
			return;
		}
		for (Element rElement : qElements) {
			QualifiedReferenceImpl q = new QualifiedReferenceImpl();
			q.setNestedParent(this);
			q.parseElement(rElement);
		}
	}
	
	//Adapter interface methods ->
	public void notifyChanged(Notification notification) {
	}

	public Notifier getTarget() {
		return null;
	}

	public void setTarget(Notifier newTarget) {
	}

	public boolean isAdapterForType(Object type) {
		return false;
	}
	//Adapter interface methods <-
	
}
