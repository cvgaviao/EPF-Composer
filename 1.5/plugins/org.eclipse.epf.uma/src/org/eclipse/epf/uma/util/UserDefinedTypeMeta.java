package org.eclipse.epf.uma.util;

import java.util.HashMap;
import java.util.Map;

public class UserDefinedTypeMeta {

	public static final String Type_Practice = "Practice";					//$NON-NLS-1$
	
	public static final String _typeName = "typeName";						//$NON-NLS-1$
	public static final String _problems = "problems";						//$NON-NLS-1$
	public static final String _mainDescription = "mainDescription";		//$NON-NLS-1$	
	public static final String _application = "application"; 				//$NON-NLS-1$
	public static final String _levelsOfAdoption = "levelsOfAdoption"; 		//$NON-NLS-1$
	public static final String _additionalInfo = "additionalInfo"; 			//$NON-NLS-1$
	
	public static String[] rteNames = {
		_typeName,
		_problems,
		_mainDescription,
		_application,
		_levelsOfAdoption,
		_additionalInfo,
	};
	
	public static UserDefinedTypeMeta newPracticeUtdpeMeta(String typeName) {
		UserDefinedTypeMeta meta = new UserDefinedTypeMeta();
		meta.setId(meta + ":" + typeName);								//$NON-NLS-1$
		return meta;
	}
	
	private Map<String, String> rteNameMap;
	
	private String id;
	public UserDefinedTypeMeta() {
	}
	
	public Map<String, String> getRteNameMap() {
		if (rteNameMap == null) {
			rteNameMap  = new HashMap<String, String>();
		}
		return rteNameMap;
	}
	
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}
}
