//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.tester.impl.testcommands;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.tester.TesterOutputUtil;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.iface.TestCommand;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessElement;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.VariabilityType;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.ibm.icu.text.SimpleDateFormat;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public abstract class TCEditMethodElementBase extends TestCommandImpl {					
	
	static class TagData {
		public Map attMap = new LinkedHashMap();
		public Map refMap = new LinkedHashMap();
	}
	
	private EClass eclass;
	private TagData tagData;
	private MethodElement methodElement;
	private Map refValueMap = new HashMap();
	private static Map eclassDataMap = new HashMap();
	private List childCommands = new ArrayList();
	private boolean isChildCommand = false;
	
	protected TestCommand newChildCommand(Class type) {
		try {
			TCEditMethodElementBase command = (TCEditMethodElementBase) type.newInstance();
			command.setOwner(getOwner());
			command.setIsChildCommand(true);
			if (localDebug) {
				System.out.println("LD> newChildCommand: " + type);
			}
			return command;
		} catch (Exception e){
			e.printStackTrace();
		}
		return null;
	}
	
	public void parse(Element element) {
		super.parse(element);
		init(element);
		parseAttributes();		
		parseValueChildren();
	}
	
	private void init(Element element) {
		String type = element.getAttribute(AT_Type);
		eclass = methodElement == null ? getEClass(type) : methodElement.eClass();
		if (eclass == null) {
			return;
		}
		tagData = (TagData) eclassDataMap.get(eclass);
		if (tagData != null) {
			return;
		}		
		tagData = new TagData();
		eclassDataMap.put(eclass, tagData);
		
		if (localDebug) {
			System.out.println("LD> eclass: " + eclass.getName());
			TesterOutputUtil.show("eclass.getEAllAttributes()", eclass.getEAllAttributes());
			TesterOutputUtil.show("eclass.getEAllReferences()", eclass.getEAllReferences());
		}
		
		addToEclassDataMap(eclass.getEAllAttributes(), tagData.attMap);
		addToEclassDataMap(eclass.getEAllReferences(), tagData.refMap);
		
	}
	
	private void addToEclassDataMap(List features, Map feaMap) {
		boolean verbose = getVerbose();
		boolean isRef = false;
		if (verbose) {
			if (feaMap == tagData.attMap) {
				log("\nAttribute featues for " + "\"" + eclass.getName() + "\": ");
			} else if (feaMap == tagData.refMap) {
				log("\nReference featues for " + "\"" + eclass.getName() + "\": ");
				isRef = true;
			}
		}
		for (int i=0; i<features.size(); i++) {
			EStructuralFeature feature = (EStructuralFeature) features.get(i);			
			feaMap.put(feature.getName(), feature);
			if (verbose) {
				String line = "";
				if (isRef) {
					line += ((EReference) feature).isContainment() ?
							"contained, " : "referred , ";
					line += ((EReference) feature).isMany() ?
							"many   -> " : "single -> ";
				}
				line += feature.getName() + ": ";
				line += isRef ? ((EReference) feature).getEReferenceType().getInstanceClassName()
							: ((EAttribute) feature).getEAttributeType().getInstanceClassName();
				log(line);
			}
		}
	}
	
	protected void parseAttributes() {
		setAttribute(AT_Type, getElement().getAttribute(AT_Type));
		setAttribute(AT_Path, getElement().getAttribute(AT_Path));
		
		for (Iterator it = tagData.attMap.entrySet().iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			String at_tag = (String) entry.getKey();
			setAttribute(at_tag, getElement().getAttribute(at_tag));
		}
	}
	
	protected void parseValueChildren() {
		for (Iterator it = tagData.refMap.entrySet().iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			String childTag = (String) entry.getKey();
			setAttribute(childTag, getElement().getAttribute(childTag));
			parseChildTagAsValue(childTag);									
		}
	}

	private void parseChildTagAsValue(String childTag) {
		NodeList cnodes = getElement().getElementsByTagName(childTag);
		if (cnodes == null || cnodes.getLength() == 0) {
			return;
		}
		Element cElement = (Element) cnodes.item(0);
		cnodes = cElement.getElementsByTagName(VT_Value);	
		if (cnodes == null || cnodes.getLength() == 0) {
			return;
		}
		List valueList = new ArrayList();
		for (int i=0; i<cnodes.getLength(); i++) {
			Element vElem = (Element) cnodes.item(i);
			String value = vElem.getFirstChild().getNodeValue();
			valueList.add(value);
		}
		refValueMap.put(childTag, valueList);
	}
	
	private void setMethodElement(MethodElement e) {
		methodElement = e;
	}
	
	protected MethodElement getMethodElement() {
		return methodElement;
	}	
	
	protected TagData getTagData() {
		return tagData;
	}
	
	private boolean parseChildTagAsCommand(Element cElement, String childTag) {
		EReference feature = (EReference) tagData.refMap.get(childTag);
		if (! feature.isContainment()) {
			return false;
		}
		TCEditMethodElementBase childCommand = (TCEditMethodElementBase) newChildCommand(TCEditMethodElement.class);
		Object featureObj = methodElement.eGet(feature);
		if (feature.isMany()) {
			List ownerList = (List) featureObj;
			featureObj = UmaFactory.eINSTANCE.create(feature.getEReferenceType());
			ownerList.add(featureObj);
		} 
		childCommand.setMethodElement((MethodElement) featureObj);
		if (getVerbose()) {
			cElement.setAttribute(AT_Verbose, "true");
		}
		childCommand.parse(cElement);
		addToChildCommands(childCommand);
		return true;
	}
	
	private void parseCommandChildren() {
		for (Iterator it = tagData.refMap.entrySet().iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			String childTag = (String) entry.getKey();
			NodeList cnodes = getElement().getElementsByTagName(childTag);
			if (cnodes == null || cnodes.getLength() == 0) {
				continue;
			}
			for (int i=0; i<cnodes.getLength(); i++) {
				Element cElement = (Element) cnodes.item(i);
				parseChildTagAsCommand(cElement, childTag);
			}
		}
	}
	
	private TCExeReply executeChildCommands() {
		for (int i=0; i<childCommands.size(); i++) {
			TestCommand command = (TestCommand) childCommands.get(i);
			TCExeReply result = command.execute();
			if (result != null && !result.passing()) {
				return result;
			}
		}
		return null;
	}
	
	public TCExeReply execute() {		
		MethodLibrary currLib = getOwner().getCurrentBaseLib();		
		String path = getAttribute(AT_Path);
		if (methodElement == null) {
			if (UmaFactory.eINSTANCE.create(eclass) instanceof ProcessElement) {
				methodElement = TesterOutputUtil.getProcessElement(currLib, path);
			} else {
				methodElement = TesterOutputUtil.getMethodElement(currLib, path);
			}
		}
		if (methodElement == null) {
			return null;
		}
		parseCommandChildren();
		
		eSetByAttributes();
		eSetByValueChildren();
		
		executeChildCommands();
		
		if (! getIsChildCommand()) {
			save(methodElement);
			//save((MethodElement) methodElement.eContainer());
		}

		return null;
	}

	private void eSetByAttributes() {
		Map attMap = getTagData().attMap;
		Map attributeMap = getAttributeMap();
		for (Iterator it = attMap.entrySet().iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			String at_tag = (String) entry.getKey();
			EAttribute eatt = (EAttribute) entry.getValue();
			EDataType dataType = eatt.getEAttributeType();
			String at_val = (String) attributeMap.get(at_tag);
			if (localDebug) {
				System.out.println("LD> at_tag: " + at_tag);
				System.out.println("LD> at_val: " + at_val);
				System.out.println("LD> eatt: " + eatt);
				System.out.println("LD> dataType: " + dataType);
				System.out.println("");
			}			
			if (at_val == null || at_val.length() == 0) {
				continue;
			}
			Object newValue = getESetValue(dataType, at_val);
			
			if (newValue != null) {
				methodElement.eSet(eatt, newValue);
			} else {
				log("\nWarning: not handled dataType: " + dataType.getInstanceClassName());
			}
		}
		EAttribute dateAtt = (EAttribute) attMap.get("changeDate");
		if (dateAtt != null) {
			methodElement.eSet(dateAtt, new Date());
		}
	}
	
	private Object getESetValue(EDataType dataType, String strVal) {
		Class cls = dataType.getInstanceClass();		
		if (cls.equals(String.class)) {
			return strVal;
		}
		if (cls.equals(Boolean.class)) {
			boolean b = strVal.equalsIgnoreCase("true");
			if (!b && !strVal.equalsIgnoreCase("false")) {
				throw new UnsupportedOperationException();
			}
			return new Boolean(b);
		}
		if (cls.equals(Date.class)) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
			Date date = null;
			try {
				date = sdf.parse(strVal);
			} catch (Exception e){
				e.printStackTrace();
			}
			return date;
		}
		if (cls.equals(VariabilityType.class)) {
			if (strVal.equals("contributes")) {
				return VariabilityType.CONTRIBUTES;
			}
			if (strVal.equals("extends")) {
				return VariabilityType.EXTENDS;
			}
			if (strVal.equals("replaces")) {
				return VariabilityType.REPLACES;
			}
		}
		return null;
	}
	
	private void eSetByValueChildren() {
		MethodLibrary currLib = getOwner().getCurrentBaseLib();	
		Map refMap = getTagData().refMap;
		for (Iterator it = refMap.entrySet().iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			String ref_tag = (String) entry.getKey();
			EReference eref = (EReference) entry.getValue();
			EClassifier eType = eref.getEType();
			List valueList = (List) refValueMap.get(ref_tag);
			if (localDebug) {
				System.out.println("LD> ref_tag: " + ref_tag);
				System.out.println("LD> valueList: " + valueList);
				System.out.println("LD> eref: " + eref);
				System.out.println("LD> eType: " + eType);
				System.out.println("");
			}
			
			int sz = valueList == null ? 0 : valueList.size();
			if (sz == 0) {
				continue;
			}
			if (eref.isMany()) {
				List ownerList = (List) methodElement.eGet(eref);
				for (int i=0; i<sz; i++) {
					String refPath = (String) valueList.get(i);
					MethodElement refElement = TesterOutputUtil.getMethodElement(currLib, refPath);
					if (refElement != null) {
						ownerList.add(refElement);
					}
				}
			} else if (sz == 1){
				String refPath = (String) valueList.get(0);
				MethodElement refElement = TesterOutputUtil.getMethodElement(currLib, refPath);
				if (eref.getName().equals("variabilityBasedOnElement") 
						&& refElement instanceof ProcessComponent) {
					refElement = ((ProcessComponent)refElement).getProcess();
				}
				methodElement.eSet(eref, refElement);
			} else {
				throw new UnsupportedOperationException();
			}
			

		}
	}

	protected void setIsChildCommand(boolean b) {
		isChildCommand = b;
	}
	
	protected boolean getIsChildCommand() {
		return isChildCommand;
	}
	
	protected void addToChildCommands(TestCommand command) {
		childCommands.add(command);
	}
}


