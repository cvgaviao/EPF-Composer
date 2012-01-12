package org.eclipse.epf.library.edit.meta;

import org.w3c.dom.Element;

public interface IMetaDef {

	public static final String MODIFIED_TYPE = "modifiedType"; //$NON-NLS-1$
	
	public static final String NAME = "name"; //$NON-NLS-1$

	public static final String ID = "id"; //$NON-NLS-1$
	
	public IMetaDef parseElement(Element element)	throws TypeDefException;
		
}
