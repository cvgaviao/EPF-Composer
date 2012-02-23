package org.eclipse.epf.library.edit.meta.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.library.edit.meta.IMetaDef;
import org.eclipse.epf.library.edit.meta.TypeDefException;
import org.eclipse.epf.library.edit.meta.TypeDefParser;
import org.eclipse.epf.library.edit.meta.TypeDefUtil;
import org.eclipse.epf.uma.util.MetaElement;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TypeDefParserImpl implements TypeDefParser {

	public TypeDefParserImpl() {		
	}
	
	public List<IMetaDef> parse(Document doc) throws TypeDefException {
		List metaList = new ArrayList<IMetaDef>();		
		NodeList list = doc.getElementsByTagName(IMetaDef.MODIFIED_TYPE);
		int size = list.getLength();

		Map<String, ModifiedTypeMetaImpl> map = new HashMap<String, ModifiedTypeMetaImpl>();
		for (int i = 0; i < size; i++) {
			Element element = (Element) list.item(i);
			ModifiedTypeMetaImpl meta = new ModifiedTypeMetaImpl();
			meta.parseElement(element);
			metaList.add(meta);			
			map.put(meta.getId(), meta);			
		}
		
		for (ModifiedTypeMetaImpl meta : (List<ModifiedTypeMetaImpl>) metaList) {
			try {
				Class cls = Class.forName(meta.getId());
				if (cls == null) {
					continue;
				}
				while (cls != null) {
					ModifiedTypeMetaImpl superMeta = map.get(meta.getId());
					if (superMeta != null) {
						meta.setSuperMeta(superMeta);
						break;
					}
					cls = TypeDefUtil.getSuperClass(cls);
				}				
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		for (ModifiedTypeMetaImpl meta : (List<ModifiedTypeMetaImpl>) metaList) {
			meta.processInheritance();
		}
		
		for (ModifiedTypeMetaImpl meta : (List<ModifiedTypeMetaImpl>) metaList) {
			ModifiedTypeMeta linkedMeta = getLinkedMeta(meta, map);
			meta.processLink(linkedMeta);
		}
		
		return metaList;
	}
	
	private ModifiedTypeMeta getLinkedMeta(ModifiedTypeMeta meta, Map<String, ModifiedTypeMetaImpl> map) {
		if (meta.getId().equals("org.eclipse.epf.uma.TaskDescriptor")) {		//$NON-NLS-1$
			return map.get("org.eclipse.epf.uma.Task");
		}
		if (meta.getId().equals("org.eclipse.epf.uma.RoleDescriptor")) {		//$NON-NLS-1$
			return map.get("org.eclipse.epf.uma.Role");
		}
		if (meta.getId().equals("org.eclipse.epf.uma.WorkProductDescriptor")) {		//$NON-NLS-1$
			return map.get("org.eclipse.epf.uma.WorkProduct");
		}		
		return null;
	}
	
}
