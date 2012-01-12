package org.eclipse.epf.library.edit.meta;

import org.w3c.dom.Element;

public interface TypeDefParser {

	public ITypeDef parse(Element element) throws TypeDefException;
	
}
