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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.eclipse.epf.library.tester.iface.TestTracer;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessElement;
import org.eclipse.epf.uma.ProcessPackage;

/**
 * Utility class for tester output manipulations
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TesterOutputUtil {
	
	private static TestTracer tracer;
	public static void setTracer(TestTracer t) {
		tracer = t;
	}
	
	public static void trace(String line) {
		if (tracer == null) {
			System.out.println(line);
		} else {
			tracer.trace(line);
		}
	}
	
	private static boolean localDebug = false;
	
	public static MethodPlugin getMethodPlugin(MethodLibrary lib, String name) {
		Map nameMap = getMethodElementNameMap(lib.getMethodPlugins());
		return (MethodPlugin) nameMap.get(name);
	}
	
	public static MethodConfiguration getMethodConfiguration(MethodLibrary lib, String name) {
		Map nameMap = getMethodElementNameMap(lib.getPredefinedConfigurations());
		return (MethodConfiguration) nameMap.get(name);
	}
	
	public static MethodElement getMethodElement(MethodLibrary lib, String path) {
		return getMethodElement_(lib, path,true);
	}
	
	private static MethodElement getMethodElement_(MethodLibrary lib, String path, boolean resolveProcessElement) {
		MethodElement element = getMethodElement_(lib, path);
		if (element instanceof ProcessPackage && resolveProcessElement) {
			ProcessPackage pkg = (ProcessPackage) element;
			List children = pkg.getProcessElements();
			if (children.size() == 1 && 
					((ProcessElement) children.get(0)).getName().equals(pkg.getName())) {
				return (MethodElement) children.get(0);
			}
		}
		return element;
	}
	
	private static MethodElement getMethodElement_(MethodLibrary lib, String path) {
		if (path == null || path.length() == 0) {
			return lib;
		}
		List paths = parseMethodElementPath(path);
		int sz = paths.size();
		if (sz == 0) {
			return null;
		}		
		MethodElement me = getFirstMethodElement(lib, (String) paths.get(0));
		if (me == null) {
			return null;
		}
		for (int i=1; i<sz; i++) {
			Map nameMap = getMethodElementNameMap(me.eContents());
			me = (MethodElement) nameMap.get(paths.get(i));
			if (me == null) {
				return null;
			}
		}		
		return me;
	}
	
	private static MethodElement getFirstMethodElement(MethodLibrary lib, String firstPath) {
		MethodElement me = getMethodPlugin(lib, firstPath);
		if (me != null) {
			return me;
		}
		return getMethodConfiguration(lib, firstPath);
	}
	
	public static ProcessElement getProcessElement(MethodLibrary lib, String path) {
		int ix = path.lastIndexOf("/");
		String parentPath = path.substring(0, ix);
		String name = path.substring(ix + 1);
		
		ProcessPackage outerParent = getProcessElementParent(lib, parentPath);
		
		Map nameMap = getMethodElementNameMap(outerParent.getChildPackages());
		ProcessPackage innerParent = (ProcessPackage) nameMap.get(name);
		if (innerParent instanceof ProcessComponent) {
			return ((ProcessComponent) innerParent).getProcess();
		}
		List pes = innerParent.getProcessElements();
		if (pes.size() != 1) {
			System.out.println("LD> pes.size() == " + pes.size() + " != 1");
		}
		
		return (ProcessElement) pes.get(0);
	}
	
	public static ProcessPackage getProcessElementParent(MethodLibrary lib, String parentPath) {
			return (ProcessPackage) getMethodElement_(lib, parentPath, false);	
	}
	
	public static List parseMethodElementPath(String path) {
		List ret = new ArrayList();		
		String name = path;
		while (true) {
			int ix = path.indexOf("/");
			if (ix < 0) {
				ret.add(path);
				break;
			}
			ret.add(path.substring(0, ix));
			path = path.substring(ix + 1);
		}	
		return ret;
	}
	
	private static Map getMethodElementNameMap(List contents) {
		Map nameMap = new HashMap();
		for (int i=0; i<contents.size(); i++) {
			Object obj = contents.get(i);
			if (obj instanceof MethodElement) {
				MethodElement elem = (MethodElement) obj;
				nameMap.put(elem.getName(), elem);
			}
		}
		return nameMap;
	}
	
	public static String getOutputPath(MethodElement me) {
		if (me instanceof MethodLibrary) {
			return "";
		}
		if (me instanceof MethodPlugin || me instanceof MethodConfiguration) {
			return me.getName();
		}
		Stack stack = new Stack();
		MethodElement currMe = me;
		while (! (currMe instanceof MethodLibrary)) {
			String name = currMe.getName();
			if (name != null && name.length() > 0) {
				stack.push(name);
			}
			MethodElement oldMe = currMe;
			currMe = (MethodElement) currMe.eContainer();
			if (currMe == null ) {
				if (localDebug) {
					System.out.println("LD> currMe == null, oldMe: " + oldMe);
				}
				break;
			}
		}
		String path = "";
		while (! stack.isEmpty()) {
			path += (String) stack.pop() + "/";			
		}
		int ix = path.length() - 1;
		return path.substring(0, ix);
	}
		
	public static void show(String msg, List list) {
		trace(msg);
		for (int i=0; i<list.size(); i++) {
			trace("list[" + i + "]" + list.get(i));
		}
		trace("");
	}
		
}


