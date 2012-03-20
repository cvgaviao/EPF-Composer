package org.eclipse.epf.library.edit.meta.internal;

import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.uma.util.ExtendedOpposite;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.MetaElement;
import org.w3c.dom.Element;

public class ExtendedOppositeImpl  extends MetaElementImpl implements ExtendedOpposite  {

	private boolean publish;
	public ExtendedOppositeImpl(MetaElement parent) {
		super(parent);
	}

	@Override
	public ExtendedReference getTargetReference() {
		return (ExtendedReference) getParent();
	}

	@Override
	public void parseElement(Element element) throws TypeDefException {
		super.parseElement(element);	
		if (element == null) {
			return;
		}
		if (super.publish()) {	//Default value for super is true, but we want it to be false here
			String str = element.getAttribute(IMetaDef.publish);
			publish = str == null ? true : Boolean.parseBoolean(str.trim());
		}
	}
		
	@Override
	public boolean publish() {
		return publish;
	}
	
}
