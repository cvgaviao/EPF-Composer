package org.eclipse.epf.uma.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;

public class QualifiedReferences {

	public static final String scopeSeperator = "::"; 							//$NON-NLS-1$
	private Set<EReference> qualifiedReferences;
	private ExtendedReference parent;

	private boolean qualifiedReferencesLoaded = false;

	private Map<String, String> referenceQualifiedNameToIdMap;
	private Map<String, String> referenceQualifiedIdToNameMap;
	private String qualifiedIdStr;
	private String qualifiedNameStr;

	public QualifiedReferences(ExtendedReference parent) {
		this.parent = parent;
	}

	public void setQualifiers(String qualifiedIdStr, String qualifiedNameStr) {
		this.qualifiedIdStr = qualifiedIdStr;
		this.qualifiedNameStr = qualifiedNameStr;	
	}

	public boolean isQualifiedRefernce(EReference ref) {
		return getQualifiedReferences().contains(ref);
	}

	public Set<EReference> getQualifiedReferences() {
		if (qualifiedReferences == null) {
			qualifiedReferences = new LinkedHashSet<EReference>();
		}
		String idStrValue = qualifiedIdStr;
		String nameStrValue = qualifiedNameStr;
		if (idStrValue == null || idStrValue.trim().length() == 0) {
			qualifiedReferencesLoaded = false;
			if (!qualifiedReferences.isEmpty()) {
				qualifiedReferences.clear();
			}
		} else if (!qualifiedReferencesLoaded) {
			if (!qualifiedReferences.isEmpty()) {
				qualifiedReferences.clear();
			}
			List<String> refIds = convertStringsToList(idStrValue);
			List<String> refNames = convertStringsToList(nameStrValue);
			if (refIds.size() != refNames.size()) {
				refNames = refIds;
			}
			getReferenceQualifiedNameToIdMap().clear();
			getReferenceQualifiedIdToNameMap().clear();
			for (int i = 0; i < refIds.size(); i++) {
				String refId = refIds.get(i);
				if (parent != null) {
					refId = parent.getId() + scopeSeperator + refId;
				}
				String refName = refNames.get(i);
				getReferenceQualifiedNameToIdMap().put(refName, refId);
				getReferenceQualifiedIdToNameMap().put(refId, refName);
				EReference ref = EcoreFactory.eINSTANCE.createEReference();
				ref.setName(refId);
				qualifiedReferences.add(ref);
			}
			qualifiedReferencesLoaded = true;
		}
		return qualifiedReferences;
	}

	private Map<String, String> getReferenceQualifiedNameToIdMap() {
		if (referenceQualifiedNameToIdMap == null) {
			referenceQualifiedNameToIdMap = new HashMap<String, String>();
		}
		return referenceQualifiedNameToIdMap;
	}

	private Map<String, String> getReferenceQualifiedIdToNameMap() {
		if (referenceQualifiedIdToNameMap == null) {
			referenceQualifiedIdToNameMap = new HashMap<String, String>();
		}
		return referenceQualifiedIdToNameMap;
	}

	public String getReferenceQualifierName(String referenceQualifierId) {
		getQualifiedReferences();
		return getReferenceQualifiedIdToNameMap().get(referenceQualifierId);
	}

	public String getReferenceQualifierId(String referenceQualifierName) {
		getQualifiedReferences();
		return getReferenceQualifiedNameToIdMap().get(referenceQualifierName);
	}

	public String[] getReferenceQualifierNames() {
		getQualifiedReferences();
		Set<String> nameSet = getReferenceQualifiedNameToIdMap().keySet();
		List<String> nameList = new ArrayList<String>(nameSet);
		Collections.sort(nameList);
		return nameList.toArray(new String[0]);
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

}
