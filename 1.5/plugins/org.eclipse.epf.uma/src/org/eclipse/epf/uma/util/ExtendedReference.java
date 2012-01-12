package org.eclipse.epf.uma.util;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;

public interface ExtendedReference extends MetaElement {
	
	public EReference getReference();
	
	public Set<EReference> getQualifiedReferences();
	
}
