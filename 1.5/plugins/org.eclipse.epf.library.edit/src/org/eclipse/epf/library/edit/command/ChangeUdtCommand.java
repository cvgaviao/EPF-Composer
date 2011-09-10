package org.eclipse.epf.library.edit.command;

import java.util.List;

import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.util.MethodElementPropUtil;
import org.eclipse.epf.library.edit.util.PracticePropUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Practice;

public class ChangeUdtCommand extends MethodElementSetPropertyCommand {
	
	private boolean remove = false;
	private List<Practice> items;
	
	public ChangeUdtCommand(MethodElement element, List<Practice> items, boolean remove) {
		super(element, PracticePropUtil.Practice_UtdData);
		this.items = items;
		this.remove = remove;
	}

	@Override
	public void redo() {
		MethodElementPropUtil propUtil = MethodElementPropUtil.getMethodElementPropUtil();		
		List<Practice> listValue = propUtil.getUdtList(element, true);
		if (listValue == null || items == null || items.isEmpty()) {
			return;
		}
		if (remove) {
			listValue.removeAll(items);	
		} else {
			listValue.addAll(items);	
		}
		try {
			this.value = propUtil.getReferencesXml(this.element, false);
		} catch (Exception e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}		
		super.redo();
	}
		
	@Override
	public void undo() {
		super.undo();
	}
	
}
