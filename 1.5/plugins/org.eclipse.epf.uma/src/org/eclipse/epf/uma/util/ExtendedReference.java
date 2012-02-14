package org.eclipse.epf.uma.util;

import java.util.List;

import org.eclipse.emf.ecore.EReference;

public interface ExtendedReference extends MetaElement {
		
	public EReference getReference();
	
	public List<QualifiedReference> getQualifiedReferences();
	
}
