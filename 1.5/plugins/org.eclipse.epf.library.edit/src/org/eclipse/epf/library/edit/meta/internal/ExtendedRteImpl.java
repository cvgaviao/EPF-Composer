package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefUtil;
import org.eclipse.epf.uma.util.ExtendedRte;
import org.eclipse.epf.uma.util.QualifiedReference;
import org.eclipse.epf.uma.util.UmaUtil;
import org.w3c.dom.Element;

public class ExtendedRteImpl  extends MetaElementImpl implements ExtendedRte {

	public ExtendedRteImpl() {		
	}

	public void parseElement(Element element)	throws TypeDefException {
		super.parseElement(element);
	}
	
}
