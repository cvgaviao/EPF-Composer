package org.eclipse.epf.library.edit.meta;

import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.util.ExtendedTable;

public class ReferenceTable {

	private MethodElement owner;
	private ExtendedTable meta;

	public ReferenceTable(MethodElement owner, ExtendedTable meta) {
		this.owner = owner;
		this.meta = meta;
	}	
	
	public ExtendedTable getMeta() {
		return meta;
	}
	
	public static ReferenceTable convertToTable(MethodElement element, ExtendedTable meta, String guidListString) {
		String[] guids = guidListString.split(MethodElementPropUtil.infoSeperator);
		int sz = guids == null ? 0 : guids.length;
		if (sz < 3) {
			return null;
		}
		int tupleSz = sz / 3;
		for (int i = 0; i < tupleSz; i++) {
			int j = i *3;
			String row = guids[j + 0];
			String col = guids[j + 1];
			String cel = guids[j + 2];
		}						
		
		return null;
	}
	
	public static String convertToString(ReferenceTable table) {
				
		
		return null;
	}
	
	
}
