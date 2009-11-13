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

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.tester.TesterOutputUtil;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ecore.EProperty;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCOutputMethodElement extends TestCommandImpl {

	//May want to use feature classes directly instead of names later
	private static String[] excludedFeatures = {
		//"guid",
		//"gUIDs"
	};
	
	private static Set excludedFeatureSet = new HashSet();
	
	static {
		for (int i=0; i<excludedFeatures.length; i++) {
			excludedFeatureSet.add(excludedFeatures[i]);
		}
	}
	
	private static boolean localDebug = false;
	String path;
	private Document doc;
	private Map systemPkgMap = new HashMap();
	private Element root;
	private boolean recursive = true;
	private String tagName;
	
	public void parse(Element element) {
		super.parse(element);
		doc = getOwner().getOutputDocument();
		//String tagName = tagName(); need to fix tagName() mehtod
		tagName = element.getTagName();
		
		setAttribute(AT_Path, element.getAttribute(AT_Path));
		setAttribute(AT_Recursive, element.getAttribute(AT_Recursive));
		setAttribute(AT_OutputFileName, element.getAttribute(AT_OutputFileName));	
	}		
	
	public TCExeReply execute() {	
		path = getAttribute(AT_Path);
		recursive = parseBoolean(AT_Recursive, recursive);
		
		MethodLibrary lib = getOwner().getCurrentBaseLib();
		root = doc.getDocumentElement();
				
		if (tagName.equals(TestCommandMgr.TCOutputLibrary)) {
			outputMethodElement(lib, root);
		} else if (tagName.equals(TestCommandMgr.TCOutputPlugin)) {
			MethodPlugin plugin = TesterOutputUtil.getMethodPlugin(lib, path);
			outputMethodElement(plugin, root);
		} else if (tagName.equals(TestCommandMgr.TCOutputConfiguration)) {
			MethodConfiguration config = TesterOutputUtil.getMethodConfiguration(lib, path);
			outputMethodElement(config, root);
		} else if (tagName.equals(TestCommandMgr.TCOutputMethodElement)) {
			MethodElement me = TesterOutputUtil.getMethodElement(lib, path);
			outputMethodElement(me, root);
		} else {
			throw new UnsupportedOperationException();
		}

		outputToFile();		
		
		return null;
	}

	private void outputToFile() {
		String outputFileName = getAttribute(AT_OutputFileName);
		if (outputFileName == null || outputFileName.length() == 0) {
			return;
		}
		String outputPath = getOwner().getOutputPath();
		File file = new File(outputPath).getParentFile();
		outputPath = file.getAbsolutePath() + File.separator + outputFileName;
		
		Document outputDocument = getOwner().getOutputDocument();
		try {
			XMLUtil.saveDocument(outputDocument, outputPath);
		} catch (Exception e){
			e.printStackTrace();
		}
	}
	
	private void outputMethodElement(MethodElement element, Element parentNode) {	
		Element node = doc.createElement(getType(element));		
		parentNode.appendChild(node);
		if (parentNode == root) {
			node.setAttribute("path", path);
		}
		outputFeatures(element, node);
	}
	
	private void outputReference(EReference feature, MethodElement element, Element parentNode) {
		Element node = doc.createElement(feature.getName());
		parentNode.appendChild(node);
		node.setAttribute("path", TesterOutputUtil.getOutputPath(element));
	}
	
	private void outputFeatures(MethodElement element, Element node) {
		List properties = element.getInstanceProperties();		
		for (int i = 0; i < properties.size(); i++) {
			EProperty ep = (EProperty) properties.get(i);
			EStructuralFeature feature = ep.getEStructuralFeature();
			if (excludedFeatureSet.contains(feature.getName())) {
				continue;
			}
			Object value = element.eGet(feature);
			if (value == null) {
				continue;
			}
			if (feature instanceof EReference) {
				if (! recursive && node.getParentNode() != root) {
					continue;
				}
				EReference refFeature = (EReference) feature;
				if (value instanceof List) {
					outputListFeature(refFeature, (List) value, node);
				} else if (value instanceof MethodElement) {
					if (refFeature.isContainment()) {
						outputMethodElement((MethodElement) value, node);
					} else {
						outputReference(refFeature, (MethodElement) value, node);
					}
				} else if (localDebug){
					getOwner().trace("Warning> value is of non-output type: " + value.getClass().getName());
				}
			} else if (feature instanceof EAttribute) {
				node.setAttribute(feature.getName(), value.toString());
			} else {
				throw new UnsupportedOperationException();
			}
			
		}			
	}
	
	private void outputListFeature(EReference feature, List list, Element parentNode) {
		int sz = list == null ? 0 : list.size();
		if (sz == 0) {
			return;
		}
		if (! (list.get(0) instanceof MethodElement)) {
			getOwner().trace("Warning> list.get(0) is of type: " + list.get(0).getClass().getName());
			return;
		}
		boolean containment = feature.isContainment();
		for (int i=0; i<sz; i++) {
			MethodElement element = (MethodElement) list.get(i);
			if (element == null) {
				continue;
			}
			if (containment) {
				outputMethodElement(element, parentNode);
			} else {
				outputReference(feature, element, parentNode);
			}
		}
	}	
	
	private String getType(Object obj) {
		String clsName = obj.getClass().getName();
		int ix = clsName.lastIndexOf(".") + 1;
		String type = clsName.substring(ix);
		ix = type.lastIndexOf("Impl");
		if (obj instanceof MethodElement && ix + 4 ==  type.length()) {
			type = type.substring(0, ix);
		}
		return type;
	}	
	
	
	
}


