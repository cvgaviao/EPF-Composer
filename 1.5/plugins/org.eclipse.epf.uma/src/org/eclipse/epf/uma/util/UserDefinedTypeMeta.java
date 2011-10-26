package org.eclipse.epf.uma.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;
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
	public static final String _referenceQualifierNames = "referenceQualifierNames";//$NON-NLS-1$
	
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
		_referenceQualifiers,
		_referenceQualifierNames
	};
	
	
	private Map<String, String> rteNameMap;
	
	private String id;
	
	private Set<EReference> qualifiedReferences;
	
	private boolean qualifiedReferencesLoaded = false;

	public static UserDefinedTypeMeta newPracticeUtdpeMeta(String typeName) {
		UserDefinedTypeMeta meta = new UserDefinedTypeMeta();
		meta.setId(getPracticeUdtId(typeName));
		return meta;
	}
	
	public static String getPracticeUdtId(String typeName) {
		return Type_Practice + ":" + typeName;			//$NON-NLS-1$
	}
					
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
	
	public List<String> getReferenceQualifiers() {
		String value = getRteNameMap().get(_referenceQualifiers);
		if (value == null || value.trim().length() == 0) {
			return null;
		}
		return convertStringsToList(value);
	}
		
	private List<String> convertStringsToList(String string) {
		ArrayList<String> strList = new ArrayList<String>();
		if (string == null)
			string = ""; //$NON-NLS-1$
		String strings[] = string
				.split("\\,"); 		//$NON-NLS-1$);
		for (int i = 0; i < strings.length; i++) {
			if (strings[i].trim().length() > 0)
				strList.add(strings[i].trim());
		}
		return strList;
	}
	
	public boolean isQualifiedRefernce(EReference ref) {
		return getQualifiedReferences().contains(ref);
	}
	
	public Set<EReference> getQualifiedReferences() {
		if (qualifiedReferences == null) {
			qualifiedReferences = new LinkedHashSet<EReference>();
		}
		String value = getRteNameMap().get(_referenceQualifiers);
		if (value == null || value.trim().length() == 0) {
			qualifiedReferencesLoaded = false;
			if (!qualifiedReferences.isEmpty()) {
				qualifiedReferences.clear();
			}
		} else if (!qualifiedReferencesLoaded) {
			if (!qualifiedReferences.isEmpty()) {
				qualifiedReferences.clear();
			}
			List<String> refNames = convertStringsToList(value);
			for (String refName : refNames) {
				EReference ref = EcoreFactory.eINSTANCE.createEReference();
				ref.setName(refName);
				qualifiedReferences.add(ref);
			}
			qualifiedReferencesLoaded = true;
		}
		return qualifiedReferences;
	}
	
}
