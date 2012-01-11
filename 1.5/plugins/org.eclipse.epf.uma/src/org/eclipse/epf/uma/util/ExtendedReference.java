package org.eclipse.epf.uma.util;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;

public class ExtendedReference {

	private String name;
	private String id;
	private EReference ref;	
//	private ExtendedReference parent;		//for future nested extended reference structure

	private QualifiedReferences qReferences;
	private Set<EReference> scopedQualifiedReferences;

	public String getName() {
		return name;
	}

	public ExtendedReference() {		
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}
	
	public EReference getReference() {
		return ref;
	}
	
	public Set<EReference> getQualifiedReferences() {
		return qReferences == null ? Collections.EMPTY_SET : qReferences.getQualifiedReferences();
	}
		
}
