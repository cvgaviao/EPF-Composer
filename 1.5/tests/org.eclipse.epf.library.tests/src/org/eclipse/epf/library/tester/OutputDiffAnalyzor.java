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
package org.eclipse.epf.library.tester;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.tester.iface.TestTracer;
import org.eclipse.epf.uma.MethodElement;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Class to analyze the differneces between two library tester outputs
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class OutputDiffAnalyzor {
	
	private boolean localDebug = false;
	private TestTracer tracer;
	private boolean needToAnalyze = true; 
	private File f1;
	private File f2;
	private Document d1;
	private Document d2;
	private int diffCount = 0;
	private int elemComparedCount = 0;
	
	private static boolean excludeGuidFromName = true;
	//May want to use feature classes directly instead of names later
	private static String[] excludedFeatures = {
		"guid",
		"changeDate"
	};
	
	private static Set excludedFeatureSet = new HashSet();
	
	static {
		for (int i=0; i<excludedFeatures.length; i++) {
			excludedFeatureSet.add(excludedFeatures[i]);
		}
	}
	
	public OutputDiffAnalyzor(TestTracer t, Document d1, Document d2, File f1, File f2) {
		tracer = t;
		this.f1 = f1;
		this.f2 = f2;
		this.d1 = d1 == null ? getDocument(f1) : d1;
		this.d2 = d2 == null ? getDocument(f2) : d2;
	}		
	
	private Document getDocument(File file) {
		if (file.exists()) {
			try {
				return XMLUtil.loadXml(file);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return null;
	}
	
	private void trace(String line) {
		tracer.trace(line);
	}
	
	private void analyze() {
		trace("OutputDiffAnalyzor.analyze ->");
		trace("f1: " + f1.getAbsolutePath());
		trace("f2: " + f2.getAbsolutePath());
		trace("");		
		analyze_();
		trace("OutputDiffAnalyzor.analyze <-");
	}
	
	private void analyze_() {
		if (! needToAnalyze) {
			trace("no need to analyse");
			return;
		}
		compareRoot(d1.getDocumentElement(), d2.getDocumentElement());
		needToAnalyze = false;
	}
	
//	Top entry for method element comparison
	public void compareRoot(Element root1,  Element root2) {		
		diffCount = 0;
		elemComparedCount = 1;		
		trace("");
		trace("compareRoot -> ");
	
		List elementList1 = collectElements(root1.getChildNodes());
		List elementList2 = collectElements(root2.getChildNodes());		
		int sz1 = elementList1.size();
		int sz2 = elementList2.size();

		if (sz1 != sz2) {
			String msg = "root child elements: sz1 != sz2 -> " + Integer.toString(sz1) + " != " + Integer.toString(sz2);
			logDiff(msg, root1, root2);
		}
		
		int sz = sz1 < sz2 ? sz1 : sz2;
		for (int i=0; i<sz; i++) {
			compare((Element) elementList1.get(i), (Element) elementList2.get(i));
		}
		
		trace("compareRoot <- diffs: " + diffCount + ", elements: " + elemComparedCount + "\n");
	}
	
	private List collectElements(NodeList nodeList) {
		List elements = new ArrayList();
		for (int i=0; i<nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element) {
				elements.add(node);
			}
		}		
		return elements;
	}
	
	private void compare(Element node1,  Element node2) {
		trace("compare -> ");
		compareInner(node1, node2);
		trace("compare <- ");
	}
	
	private void compareInner(Element node1,  Element node2) {
		elemComparedCount++;
		if (localDebug) {
			trace("compareInner, node1: " + outputString(node1));
			trace("compareInner, node2: " + outputString(node1));
			trace("");
		}
		
		attributeEquals(node1, node2);
			
		List elementList1 = collectElements(node1.getChildNodes());
		List elementList2 = collectElements(node2.getChildNodes());		
		int sz1 = elementList1.size();
		int sz2 = elementList2.size();

		if (sz1 != sz2) {
			String msg = "Child elements: sz1 != sz2 -> " + Integer.toString(sz1) + " != " + Integer.toString(sz2);
			logDiff(msg, node1, node2);
		}
		
		int sz = sz1 < sz2 ? sz1 : sz2;
		for (int i=0; i<sz; i++) {
			compareInner((Element) elementList1.get(i), (Element) elementList2.get(i));
		}
	}

	private boolean attributeEquals(Element node1, Element node2) {
		boolean ret = true;
		
		NamedNodeMap atts1 = node1.getAttributes();
		NamedNodeMap atts2 = node2.getAttributes();
		int sz1 = atts1.getLength();
		int sz2 = atts2.getLength();
		if (sz1 != sz2) {
			logDiff("Attributes: sz1 != sz2", node1, node2);
			ret = false;
		}
		int sz = sz1 < sz2 ? sz1 : sz2;		
		for (int i=0; i<sz; i++) {
			String attName = atts1.item(i).getNodeName();
			if (excludedFeatureSet.contains(attName)) {
				continue;
			}
			String val1 = node1.getAttribute(attName);
			String val2 = node2.getAttribute(attName);
			if (attName.equals("name") && excludeGuidFromName) {
				int ix = val1.indexOf(",");
				if (ix > 0) {
					val1 = val1.substring(0, ix);
				}
				ix = val2.indexOf(",");
				if (ix > 0) {
					val2 = val2.substring(0, ix);
				}
			}
			if (! valueEquals(val1, val2)) {
				String msg = attName + " values not the same!";
				logDiff(msg, node1, node2, val1, val2);
			}
		}
		
		return ret;
	}
	
	private boolean valueEquals(Object val1, Object val2) {
		if (val1 == null) {
			val1 = "";
		}
		
		if (val2 == null) {
			val2 = "";
		}
		
		if (val1 instanceof String) {
			String str1 = (String) val1;
			String str2 = (String) val2;
			return str1.equals(str2);
		}
		
		return false;
	}
	
	
	private void logDiff(String msg, Element node1, Element node2, Object val1, Object val2) {
		log(msg, node1, node2, true);
		String prompt = getDiffPrompt();
		trace(prompt + "val1: " + val1);
		trace(prompt + "val2: " + val2);	
		trace("");
	}	
		
	private int incDiffCount() {
		return ++diffCount;
	}
	
	private String getDiffPrompt() {
		return "D_" + diffCount + "> ";	
	}	
	
	private void logDiff(String msg, Element node1, Element node2) {
		log(msg, node1, node2, true);
		trace("");
	}
	
	private void log(String msg, Element node1, Element node2, boolean diff) {
		if (diff) {
			incDiffCount();
		}
		String prompt = diff ? getDiffPrompt() : "Warning> ";
		//trace(prompt + "path: " + pathString(node1));
		trace(prompt + "msg: " + msg);
		trace(prompt + "node1: " + outputString(node1));
		trace(prompt + "node2: " + outputString(node2));
	}
	
	private String pathString(Element node) {
		Stack paths = new Stack();
		Node currNode = node;
		while (currNode != null && currNode instanceof Element) {
			String name = ((Element) currNode).getAttribute("name");
			paths.push(name);
			currNode = currNode.getParentNode();
		}
		String ret = "";
		while(! paths.isEmpty()) {
			ret += "/" + paths.pop();
		}		
		return ret;
	}
	
	public boolean compare() {
		analyze();
		return true;
	}
	
	private String outputString(Element node) {
		if (node == null) {
			return null;
		}			
		String type = node.getAttribute("type");
		return type + ": " + pathString(node);
	}
	
	public int getDiffCount() {
		return diffCount;
	}
	
	public int getElemComparedCount() {
		return elemComparedCount;
	}
	

}
