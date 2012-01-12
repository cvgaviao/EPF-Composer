package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.library.edit.meta.ITypeDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefParser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TypeDefParserImpl implements TypeDefParser {

	public TypeDefParserImpl() {		
	}
	
	public List<ITypeDef> parse(Document doc) throws TypeDefException {
		List typeDefList = new ArrayList<ITypeDef>();
		
		NodeList list = doc.getElementsByTagName(ITypeDef.MODIFIED_TYPE);
		int size = list.getLength();
		for (int i = 0; i < size; i++) {
			Element element = (Element) list.item(i);
			ITypeDef typeDef = new ModifiedTypeMetaImpl();
			typeDef.parse(element);
			typeDefList.add(typeDef);
		}				
		return typeDefList;
	}

}
