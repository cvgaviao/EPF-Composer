//------------------------------------------------------------------------------
//Copyright (c) 2005, 2007 IBM Corporation and others.
//All rights reserved. This program and the accompanying materials
//are made available under the terms of the Eclipse Public License v1.0
//which accompanies this distribution, and is available at
//http://www.eclipse.org/legal/epl-v10.html
//
//Contributors:
//IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.uma.MethodElement;
import org.w3c.dom.Document;

/**
* @author Weiping Lu
*
* @since 1.5.1.3
*/
public class XmlEditUtil {

	private MethodElementPropUtil propUtil;
	private Document doc;
	public MethodElementPropUtil getPropUtil() {
		return propUtil;
	}

	public XmlEditUtil(MethodElementPropUtil propUtil) {
		this.propUtil = propUtil;
	}
	
	public Document getDocument() throws Exception {
		if (doc == null) {
			try {
				doc = XMLUtil.createDocument();
			} catch (Exception e) {
				LibraryEditPlugin.getDefault().getLogger().logError(e);
			}
		}
		return doc;
	}
	
	public void storeToOwner(MethodElement owner, String propName)  throws Exception  {
		String value = XMLUtil.toXmlString(doc);
		propUtil.setStringValue(owner, propName, value);
	}
	
	
}
