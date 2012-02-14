package org.eclipse.epf.uma.util;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.uma.ecore.IUserDefinedTypeMeta;

public interface ModifiedTypeMeta extends MetaElement {
	
	public List<ExtendedReference> getReferences();
	public List<ExtendedAttribute> getRtes();	
	
}
