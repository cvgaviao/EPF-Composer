package org.eclipse.epf.library.edit.meta;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.edit.meta.internal.ModifiedTypeMetaImpl;
import org.eclipse.epf.library.edit.meta.internal.TypeDefParserImpl;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;

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
	
	public IMetaDef createMetaDef(Class cls) {
		if (cls == ModifiedTypeMeta.class) {
			return new ModifiedTypeMetaImpl();
		}
		return null;
	}
	
	public ExtendedReference getExtendedReference(EReference ref) {
		for (Object adapter : ref.eAdapters()) {
			if (adapter instanceof ExtendedReference) {
				return (ExtendedReference) adapter;
			}
		}
		return null;
	}
	
}
