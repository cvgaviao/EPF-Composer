package org.eclipse.epf.library.edit.meta;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.util.ExtendedTable;

public class ReferenceTable {

	private MethodElement element;

	private ExtendedTable meta;
	private Map<MethodElement, Map<MethodElement, MethodElement>> data = new HashMap<MethodElement, Map<MethodElement, MethodElement>>();

	private ReferenceTable(MethodElement owner, ExtendedTable meta) {
		this.element = owner;
		this.meta = meta;
	}
	
	public ReferenceTable(MethodElement element, ExtendedTable meta, String guidListString) {
		this(element, meta);
		String[] guids = guidListString.split(MethodElementPropUtil.infoSeperator);
		int sz = guids == null ? 0 : guids.length;
		if (sz < 3) {
			return;
		}
		
		int tupleSz = sz / 3;
		for (int i = 0; i < tupleSz; i++) {
			int j = i *3;
			String row = guids[j + 0];
			String col = guids[j + 1];
			String cel = guids[j + 2];
			
			MethodElement rowE = LibraryEditUtil.getInstance().getMethodElement(row);
			if (rowE == null) {
				continue;
			}
			MethodElement colE = LibraryEditUtil.getInstance().getMethodElement(col);
			if (colE == null) {
				continue;
			}
			MethodElement celE = LibraryEditUtil.getInstance().getMethodElement(cel);
			if (celE == null) {
				continue;
			}
			add(rowE, colE, celE);
		}						
	}
	
	public ExtendedTable getMeta() {
		return meta;
	}
	
	public void add(MethodElement row, MethodElement col, MethodElement cel) {
		Map<MethodElement, MethodElement> colCelMap = data.get(row);
		if (colCelMap == null) {
			colCelMap = new HashMap<MethodElement, MethodElement>();
			data.put(row, colCelMap);			
		}
		colCelMap.put(col, cel);
	}
	
	
	public String getGuidListString() {
		LibraryEditUtil util = LibraryEditUtil.getInstance();
		String sp = MethodElementPropUtil.infoSeperator;
		StringBuffer sb = new StringBuffer();
		for (Map.Entry<MethodElement, Map<MethodElement, MethodElement>> entry : data
				.entrySet()) {
			MethodElement row = entry.getKey();
			Map<MethodElement, MethodElement> map = entry.getValue();
			if (map != null && !map.isEmpty()
					&& row == util.getMethodElement(row.getGuid())) {

				for (Map.Entry<MethodElement, MethodElement> ccEntry : map
						.entrySet()) {
					MethodElement col = ccEntry.getKey();
					MethodElement cel = ccEntry.getValue();
					if (col != null && cel != null
							&& col == util.getMethodElement(col.getGuid())
							&& cel == util.getMethodElement(cel.getGuid())) {
						if (sp.length() != 0) {
							sb.append(sp);
						}
						sb.append(row.getGuid());
						sb.append(sp + col.getGuid());
						sb.append(sp + cel.getGuid());
					}

				}

			}
		}
		return sb.toString();
	}
	
	public MethodElement getElement() {
		return element;
	}
	
}