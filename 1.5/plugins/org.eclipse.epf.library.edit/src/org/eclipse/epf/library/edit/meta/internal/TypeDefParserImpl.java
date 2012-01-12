package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefParser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TypeDefParserImpl implements TypeDefParser {

	public TypeDefParserImpl() {		
	}
	
	public List<IMetaDef> parse(Document doc) throws TypeDefException {
		List typeDefList = new ArrayList<IMetaDef>();
		
		NodeList list = doc.getElementsByTagName(IMetaDef.MODIFIED_TYPE);
		int size = list.getLength();
		for (int i = 0; i < size; i++) {
			Element element = (Element) list.item(i);
			IMetaDef typeDef = new ModifiedTypeMetaImpl();
			typeDef.parse(element);
			typeDefList.add(typeDef);
		}				
		return typeDefList;
	}

}
