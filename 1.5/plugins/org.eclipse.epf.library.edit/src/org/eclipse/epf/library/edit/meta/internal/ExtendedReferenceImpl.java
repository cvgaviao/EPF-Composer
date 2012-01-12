package org.eclipse.epf.library.edit.meta.internal;

import java.util.Collections;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.QualifiedReferences;
import org.w3c.dom.Element;

public class ExtendedReferenceImpl extends MetaElementImpl implements ExtendedReference {
	private EReference ref;	
//	private ExtendedReference parent;		//for future nested extended reference structure

	private QualifiedReferences qReferences;
	private Set<EReference> scopedQualifiedReferences;

	public ExtendedReferenceImpl() {		
	}
	
	public EReference getReference() {
		return ref;
	}
	
	public Set<EReference> getQualifiedReferences() {
		return qReferences == null ? Collections.EMPTY_SET : qReferences.getQualifiedReferences();
	}
	
	public IMetaDef parse(Element element)	throws TypeDefException {
		return null;
	}
}
