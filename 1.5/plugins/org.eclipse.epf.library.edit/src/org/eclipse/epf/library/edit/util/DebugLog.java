package org.eclipse.epf.library.edit.util;

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.epf.common.utils.FileUtil;
import org.eclipse.epf.uma.MethodElement;

/**
 * Utility class for logging debug info
 * @author Weiping Lu
 *
 */
public class DebugLog {

	private String indent; //$NON-NLS-1$
	private static String prompt;
	public DebugLog(String prompt) {
		this.prompt = prompt;
		int n = 5 + (prompt == null ? 0 : prompt.length());
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < n; i++) {
			sb.append(" ");	//$NON-NLS-1$
		}
		indent = sb.toString();
	}
		
	public void log(String msg) {
		FileUtil.log(prompt + msg);
	}
	
	public void logElements(String label, Collection<? extends MethodElement> elements, boolean showEmpty) {
		boolean empty = elements == null || elements.isEmpty();
		if (!showEmpty && empty) {
			return;
		}
		String str = prompt + label;	//$NON-NLS-1$
		
		if (!empty) {
			for (MethodElement element : elements) {
				String line = "\n" + indent + DebugUtil.toString(element, 2);//$NON-NLS-1$
				str += line;
			}
		} else {
			str += "\n" + indent + "Empty list";		//$NON-NLS-1$ //$NON-NLS-2$
		}
		FileUtil.log(str + "\n");//$NON-NLS-1$
	}
		
}
