package org.eclipse.epf.library.edit.util;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Practice;
import org.w3c.dom.Element;

public class PracticePropUtil extends MethodElementPropUtil {
	
	public static final String Practice_UtdData = "Practice_utdData";		//$NON-NLS-1$	
	
	
	private static PracticePropUtil practicePropUtil = new PracticePropUtil();
	public static PracticePropUtil getPracticePropUtil() {
		return practicePropUtil;
	}
	
	public static PracticePropUtil getPracticePropUtil(IActionManager actionManager) {
		return new PracticePropUtil(actionManager);
	}
	
	protected PracticePropUtil() {		
	}
	
	protected PracticePropUtil(IActionManager actionManager) {
		super(actionManager);
	}
	
	public void storeUtdData(Practice practice, Map<String, String> data)  throws Exception {
		PracticeXmlEditUtil xmlEditUtil = new PracticeXmlEditUtil(practice, this);
		xmlEditUtil.storeUtdData(data);
	}
	
	public  Map<String, String> getUtdData(Practice practice)  throws Exception {
		Map<String, String> map = new HashMap<String, String>();
		PracticeXmlEditUtil xmlEditUtil = new PracticeXmlEditUtil(practice, this);
		xmlEditUtil.retrieveUtdData(map);
		return map;
	}
	
	private static class PracticeXmlEditUtil extends XmlEditUtil {
		
		public static final String _typeName = "typeName";						//$NON-NLS-1$
		public static final String _problems = "problems";						//$NON-NLS-1$
		public static final String _mainDescription = "mainDescription";		//$NON-NLS-1$	
		public static final String _application = "application"; 				//$NON-NLS-1$
		public static final String _levelsOfAdoption = "levelsOfAdoption"; 		//$NON-NLS-1$
		public static final String _additionalInfo = "additionalInfo"; 			//$NON-NLS-1$
		
		public static String[] utdNames = {
			_typeName,
			_problems,
			_mainDescription,
			_application,
			_levelsOfAdoption,
			_additionalInfo,
		};
		
		private Practice practice;
		
		public PracticeXmlEditUtil(Practice practice, MethodElementPropUtil propUtil) {
			super(propUtil);
			this.practice = practice;
		}		
		
		public void storeUtdData(Map<String, String> data) throws Exception {
			if (practice == null) {
				return;
			}
			Element firstElement = createFirstElement(Practice_UtdData);				
			for (String name : utdNames) {
				String value = data.get(name);
				if (value != null && value.length() > 0) {
					firstElement.setAttribute(name, value);
				}
			}			
			storeToOwner(practice, Practice_UtdData);	
		}
		
		public void retrieveUtdData(Map<String, String> data) throws Exception {
			data.clear();
			
			String xmlString = getPropUtil().getStringValue(practice, Practice_UtdData);
			Element firstElement = loadDocumentAndGetFirstElement(xmlString);	
				
			for (String name : utdNames) {
				String value = firstElement.getAttribute(name);
				if (value != null && value.length() > 0) {
					data.put(name, value);
				}
			}
		}
		
	}

	
	
}
