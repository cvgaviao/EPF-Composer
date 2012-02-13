package org.eclipse.epf.uma.util;

import java.util.List;

import org.eclipse.emf.ecore.EReference;

public interface ExtendedReference extends MetaElement {
	
	public ExtendedReference getNestedParent();
		
	public EReference getReference();
	
	public List<QualifiedReference> getQualifiedReferences();
	
}
