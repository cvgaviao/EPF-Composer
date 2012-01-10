package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.library.edit.command.IActionManager;

public class PropUtil extends MethodElementPropUtil {

	private static PropUtil propUtil = new PropUtil();
	public static PropUtil getPropUtil(IActionManager actionManager) {
		return new PropUtil(actionManager);
	}
	
	public static PropUtil getPropUtil() {
		return propUtil;
	}
		
	protected PropUtil() {		
	}
	
	protected PropUtil(IActionManager actionManager) {
		super(actionManager);
	}	
	
}
