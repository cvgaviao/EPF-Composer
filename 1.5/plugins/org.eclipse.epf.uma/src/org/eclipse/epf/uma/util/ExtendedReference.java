package org.eclipse.epf.uma.util;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;

public interface ExtendedReference {

	public String getName();
	
	public void setName(String name);
	
	public String getId();

	public void setId(String id);
	
	public EReference getReference();
	
	public Set<EReference> getQualifiedReferences();
	
}
