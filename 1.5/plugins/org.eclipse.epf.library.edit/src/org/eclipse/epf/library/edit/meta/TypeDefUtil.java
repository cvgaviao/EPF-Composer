package org.eclipse.epf.library.edit.meta;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.meta.internal.ModifiedTypeMetaImpl;
import org.eclipse.epf.library.edit.meta.internal.TypeDefParserImpl;
import org.eclipse.epf.library.edit.util.PropUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.ecore.EProperty;
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
	
	public void associate(ExtendedReference eRef, EReference ref) {
		if (! (eRef instanceof Adapter)) {
			return;
		}
		int sz = ref.eAdapters().size();
		for (int i = sz - 1; i >=0; i--) {
			Object adapter = ref.eAdapters().get(i);
			if (adapter instanceof ExtendedReference) {
				ref.eAdapters().remove(i);
			}
		}
		ref.eAdapters().add((Adapter) eRef);
	}
	
	public ExtendedReference getAssociatedExtendedReference(EStructuralFeature feature) {
		for (Object adapter : feature.eAdapters()) {
			if (adapter instanceof ExtendedReference) {
				return (ExtendedReference) adapter;
			}
		}
		return null;
	}
		
	public Object eGet(EObject obj, EStructuralFeature feature) {
		if (obj instanceof MethodElement) {
			MethodElement element = (MethodElement) obj;
			if (feature instanceof EReference) {
				EReference ref = (EReference) feature;
				ExtendedReference eRef = getAssociatedExtendedReference(ref);
				if (eRef != null) {
					PropUtil propUtil = PropUtil.getPropUtil();
					return propUtil.getExtendedReferenceList(element, eRef, false);
				}
			}
		}
		return obj.eGet(feature);
	}
	
}
