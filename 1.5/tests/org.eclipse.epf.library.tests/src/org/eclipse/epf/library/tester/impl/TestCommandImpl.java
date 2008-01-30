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
package org.eclipse.epf.library.tester.impl;

import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.iface.TestCommand;
import org.eclipse.epf.library.tester.impl.testcommands.TCNewMethodElement;
import org.eclipse.epf.uma.MethodElement;
import org.w3c.dom.Element;

/**
 * Used in JUnit tests 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public abstract class TestCommandImpl implements TestCommand {

	private static TCNewMethodElement newMethodElement = new TCNewMethodElement(); 
	
	private boolean verbose = false;
	private Element element;
	protected static boolean localDebug = false;
	private TestCommandMgr owner;
	private Map atttibuteMap = new LinkedHashMap();
	
	public void setOwner(TestCommandMgr mgr) {
		owner = mgr;
	}
	
	protected Map getAttributeMap() {
		return atttibuteMap;
	}
	
	public void setAttribute(String attName, String attValue) {
		if (attValue != null && attValue.length() > 0) {
			atttibuteMap.put(attName, attValue);
		}
	}
	
	public String removeAttribute(String attName) {
		return (String) atttibuteMap.remove(attName);
	}
	
	public String getAttribute(String attName) {
		return (String) atttibuteMap.get(attName);
	}
	
	public void parse(Element element) {
		this.element = element;
		setAttribute(AT_Verbose, element.getAttribute(AT_Verbose));
		verbose = parseBoolean(AT_Verbose, verbose);
	}
	
	protected boolean getVerbose() {
		return verbose;
	}
	
	protected Element getElement() {
		return element;
	}
	
	protected void parseChildren(Element element) {
	}
	
	protected TestCommandMgr getOwner() {
		return owner;
	}
	
	public abstract TCExeReply execute();
	
	public String tagName() {
		return owner == null ? null : owner.getTagName(this);
	}
	
	protected boolean parseBoolean(String att, boolean defaultValue) {
		String toCheckStr = defaultValue ? "false" : "true";
		String str = getAttribute(att);
		if (str != null && str.equalsIgnoreCase(toCheckStr)) {
			return !defaultValue;
		}
		return defaultValue;
	}

	protected int parseInteger(String att, int defaultValue) {
		if (att != null && att.length() > 0) {
			return Integer.parseInt(att);
		}
		return defaultValue;
	}
	
	public EClass getEClass(String type) {
		return newMethodElement.getEClass(type);
	}
	
	public void save(MethodElement element) {
		newMethodElement.save(element);
	}

	protected void log(String line) {
		getOwner().log(line);
	}
	
}
