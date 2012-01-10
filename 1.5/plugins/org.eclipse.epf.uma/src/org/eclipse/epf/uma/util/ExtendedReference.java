package org.eclipse.epf.uma.util;

import org.eclipse.emf.ecore.EReference;

public class ExtendedReference {
	
	private String name;
	private String id;
	private EReference ref;

	private QualifiedReferences qReferences = new QualifiedReferences();

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

	public QualifiedReferences getqReferences() {
		return qReferences;
	}
	
	public EReference getReference() {
		return ref;
	}
	
}
