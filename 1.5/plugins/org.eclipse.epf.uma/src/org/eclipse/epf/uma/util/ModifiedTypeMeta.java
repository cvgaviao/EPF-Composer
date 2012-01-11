package org.eclipse.epf.uma.util;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.uma.ecore.IUserDefinedTypeMeta;

public class ModifiedTypeMeta implements IUserDefinedTypeMeta {

	public static final String _typeId = "typeId";			//$NON-NLS-1$
	public static final ModifiedTypeMeta noneValue = new ModifiedTypeMeta();
		
	private String id;
	private List<ExtendedReference> referernces;

	public ModifiedTypeMeta() {		
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}
		
	public List<ExtendedReference> getReferernces() {
		return referernces;
	}
}
