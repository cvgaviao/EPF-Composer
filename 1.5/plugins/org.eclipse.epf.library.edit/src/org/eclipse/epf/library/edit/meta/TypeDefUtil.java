package org.eclipse.epf.library.edit.meta;

import org.eclipse.epf.library.edit.meta.internal.TypeDefParserImpl;

public class TypeDefUtil {

	private static TypeDefUtil instance = new TypeDefUtil();
	
	public static TypeDefUtil getInstance() {
		return instance;
	}

	private TypeDefUtil() {		
	}
	
	public TypeDefParser getTypeDefParser() {
		return new TypeDefParserImpl();
	}
	
	
}
