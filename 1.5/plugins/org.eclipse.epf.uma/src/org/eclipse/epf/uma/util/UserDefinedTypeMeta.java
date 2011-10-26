package org.eclipse.epf.uma.util;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.uma.ecore.IUserDefinedTypeMeta;

public class UserDefinedTypeMeta implements IUserDefinedTypeMeta {

	public static final UserDefinedTypeMeta noneValue = new UserDefinedTypeMeta();
	public static final String Type_Practice = "Practice";					//$NON-NLS-1$
	
	public static final String _typeName = "typeName";						//$NON-NLS-1$
	public static final String _problems = "problems";						//$NON-NLS-1$
	public static final String _goals = "goals";							//$NON-NLS-1$
	public static final String _background = "background";					//$NON-NLS-1$	
	public static final String _mainDescription = "mainDescription";		//$NON-NLS-1$	
	public static final String _application = "application"; 				//$NON-NLS-1$
	public static final String _levelsOfAdoption = "levelsOfAdoption"; 		//$NON-NLS-1$
	public static final String _additionalInfo = "additionalInfo"; 			//$NON-NLS-1$
	public static final String _icon = "icon";                              //$NON-NLS-1$
	public static final String _shapeIcon = "shapeIcon";                    //$NON-NLS-1$
	public static final String _referenceQualifiers = "referenceQualifiers";//$NON-NLS-1$
	
	public static String[] rteNames = {
		_typeName,
		_problems,
		_goals,
		_background,
		_mainDescription,
		_application,
		_levelsOfAdoption,
		_additionalInfo,
		_icon,
		_shapeIcon,
		_referenceQualifiers
	};
	
	public static UserDefinedTypeMeta newPracticeUtdpeMeta(String typeName) {
		UserDefinedTypeMeta meta = new UserDefinedTypeMeta();
		meta.setId(getPracticeUdtId(typeName));
		return meta;
	}
	
	public static String getPracticeUdtId(String typeName) {
		return Type_Practice + ":" + typeName;			//$NON-NLS-1$
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
	
	public boolean same(UserDefinedTypeMeta other) {
		if (other == null) {
			return false;
		}
		if (! same(this.id, other.id)) {
			return false;
		}
		for (String name : rteNames) {
			String thisValue = this.getRteNameMap().get(name);
			String otherValue = other.getRteNameMap().get(name);
			if (! same(thisValue, otherValue)) {
				return false;
			}
		}
		
		return true;
	}
	
	private boolean same(String a, String b) {
		if (a == null) {
			return b == null;
		} 
		return a.equals(b);
	}
	
}
