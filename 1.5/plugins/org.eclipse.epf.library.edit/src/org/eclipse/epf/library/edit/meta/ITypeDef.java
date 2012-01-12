package org.eclipse.epf.library.edit.meta;

import org.w3c.dom.Element;

public interface ITypeDef {

	public static final String MODIFIED_TYPE = "modifiedType"; //$NON-NLS-1$
	
	public static final String NAME = "name"; //$NON-NLS-1$

	public static final String ID = "id"; //$NON-NLS-1$
	
	public ITypeDef parse(Element element)	throws TypeDefException;
		
}
