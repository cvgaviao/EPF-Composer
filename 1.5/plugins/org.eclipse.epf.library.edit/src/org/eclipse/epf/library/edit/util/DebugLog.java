package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
		logElements(label, elements, showEmpty, true);
	}
	
	public void logElements(String label, Collection<? extends MethodElement> elements, boolean showEmpty, boolean showTags) {
		boolean empty = elements == null || elements.isEmpty();
		if (!showEmpty && empty) {
			return;
		}
		String str = prompt + label;	//$NON-NLS-1$
		
		if (!empty) {
			str += " size = " + elements.size();//$NON-NLS-1$			
			
			List<String> sortedGuids = new ArrayList<String>();
			Map<String, MethodElement> map = new HashMap<String, MethodElement>();
			for (MethodElement element : elements) {
				String guid = element.getGuid();
				sortedGuids.add(guid);
				map.put(guid, element);
			}
			
			Collections.sort(sortedGuids, Comparators.PRESENTATION_NAME_GUID_COMPARATOR);
			
			for (String guid : sortedGuids) {
				MethodElement element = map.get(guid);
				String line = "\n" + indent + toString(element, 2, showTags, indent + indent);//$NON-NLS-1$
				str += line;
			}
			
		} else {
			str += "\n" + indent + "Empty list";		//$NON-NLS-1$ //$NON-NLS-2$
		}
		FileUtil.log(str + "\n");//$NON-NLS-1$
	}
	
	public String toString(MethodElement element, int ix, boolean showTags, String tagLineInden) {
		return DebugUtil.toString(element, ix);
	}
		
}
