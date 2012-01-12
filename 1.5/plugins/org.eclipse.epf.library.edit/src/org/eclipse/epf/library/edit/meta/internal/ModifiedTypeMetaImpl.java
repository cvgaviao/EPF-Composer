package org.eclipse.epf.library.edit.meta.internal;

import java.util.List;

import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;

public class ModifiedTypeMetaImpl extends MetaElementImpl implements ModifiedTypeMeta {
		
	private List<ExtendedReference> references;

	public ModifiedTypeMetaImpl() {		
	}
		
	public List<ExtendedReference> getReferences() {
		return references;
	}
	
}
