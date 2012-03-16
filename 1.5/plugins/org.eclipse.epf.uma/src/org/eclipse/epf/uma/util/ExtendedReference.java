package org.eclipse.epf.uma.util;

import java.util.List;

import org.eclipse.emf.ecore.EReference;

public interface ExtendedReference extends ExtendedFeature {
		
	public EReference getReference();
	
	public List<QualifiedReference> getQualifiedReferences();
	
	public List<String> getValueTypes();
	
	public String getContributeTo();
	
}
