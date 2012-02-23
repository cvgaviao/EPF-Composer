package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefUtil;
import org.eclipse.epf.library.edit.uma.ExtendReferenceMap;
import org.eclipse.epf.library.edit.util.PropUtil;
import org.eclipse.epf.uma.util.ExtendedAttribute;
import org.eclipse.epf.uma.util.MetaElement;
import org.eclipse.epf.uma.util.QualifiedReference;
import org.eclipse.epf.uma.util.UmaUtil;
import org.w3c.dom.Element;

public class ExtendedAttributeImpl  extends MetaElementImpl implements ExtendedAttribute {

	private EAttribute att;	
	
	public ExtendedAttributeImpl(MetaElement parent) {
		super(parent);
	}

	public EAttribute getAttribute() {
		return att;
	}
	
	public void parseElement(Element element)	throws TypeDefException {
		super.parseElement(element);			
		att =  UmaUtil.createAttribute(getId());
		TypeDefUtil.getInstance().associate(this, att);				
	}
		
}
