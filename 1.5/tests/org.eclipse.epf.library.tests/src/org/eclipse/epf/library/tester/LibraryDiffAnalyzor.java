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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.sdo.EProperty;
import org.eclipse.epf.importing.services.ResourceScanner;
import org.eclipse.epf.library.tester.iface.TestTracer;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.Diagram;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;

/**
 * Class to analyze the differneces between two method libraries
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class LibraryDiffAnalyzor {
	
	private final boolean localDebug = false;
	private TestTracer tracer;
	private MethodLibrary lib1;
	private MethodLibrary lbi2;
	private boolean greedy = true;
	private int compareOption = 1;		//0: element1 and element2 are on equal footing, //1: element1 is a super set of element2
	private boolean needToAnalyze = true;
	private boolean compareResult = false;
	private int diffCount = 0;
	private int elemComparedCount = 0;
	private static String emptyString = "";	
	public static final Pattern emptyLine = Pattern.compile("\\s", Pattern.CASE_INSENSITIVE | Pattern.DOTALL); //$NON-NLS-1$
	
	private static boolean trim = false;
	private static  boolean skipRef = true;
	private static boolean useNameAs2ndId = false;
	private static Set skipFeatureSet = new HashSet();

	public LibraryDiffAnalyzor(TestTracer t, MethodLibrary l1, MethodLibrary l2) {
		tracer = t;
		lib1 = l1;
		lbi2 = l2;
		LibraryUtil.loadAll((MethodLibrary) lib1);
		LibraryUtil.loadAll((MethodLibrary) lbi2);
	}	
	
	public static void setTrim(boolean b) {
		trim = b;
	}
	
	public static void setSkipRef(boolean b) {
		skipRef = b;
	}
		
	public static void addExcludedFeature(String featureName) {
		skipFeatureSet.add(featureName);
	}
	
	private void analyze() {
		if (! needToAnalyze) {
			return;
		}
		compareResult = elementEquals(lib1, lbi2);
		needToAnalyze = false;
	}

	public boolean compare() {
		analyze();
		return compareResult;
	}
	
	public void setGreedy(boolean b) {
		if (greedy != b) {
			needToAnalyze = true;
		}
		greedy = b;
	}
	
	public void setCompareOption(int ix) {
		if (compareOption != ix) {
			needToAnalyze = true;
		}
		compareOption = ix;
	}
	
	public static void setUseNameAs2ndId(boolean b) {
		useNameAs2ndId = b;
	}
		
	public void trace(String line) {
		if (tracer == null) {
			return;
		}
		tracer.trace(line);
		if (localDebug) {
			System.out.println(line);
		}
	}
	
	//Top entry for comparizon
	public boolean elementEquals(MethodElement elem1,  MethodElement elem2) {
		diffCount = 0;
		elemComparedCount = 0;
		trace("");
		trace("elementEquals -> ");
		trace("elem1: " + elem1);
		trace("elem2: " + elem2 + "\n");
		compare(new ArrayList(), elem1, elem2, new HashMap());
		trace("elementEquals <- diffs: " + diffCount + ", elements: " + elemComparedCount + "\n");
		return diffCount == 0;
	}
	
	private void compare(List path, MethodElement elem1,  MethodElement elem2, Map comparedElemMap) {
		path.add(elem1);
		elemComparedCount++;
		compare_(path, elem1, elem2, comparedElemMap);
		path.remove(path.size() - 1);
	}
	
	private void compare_(List path, MethodElement elem1,  MethodElement elem2, Map comparedElemMap) {
		if (localDebug) {
			trace("path: " + pathToString(path));
		}				
		
		boolean topLevel = elem1 == lib1 && elem2 == lbi2;
		boolean checkEqualSize = compareOption == 0 && !topLevel;
		
		List contentList1 = elem1.eContents();
		List contentList2 = elem2.eContents();
		int sz1 = contentList1.size();
		int sz2 = contentList2.size();
		
		if ( checkEqualSize && sz1 != sz2) {
			String msg = "sz1 != sz2: " + sz1 + " != " + sz2;
			logWarning(msg, path, elem1, elem2);		
		}		
		
		int elemCount1 = 0;
		HashMap elementMap = new HashMap();
		HashMap elementNameMap = useNameAs2ndId ? new HashMap() : null;
		HashMap elementChildMap = useNameAs2ndId ? new HashMap() : null;
		for (int i=0; i < sz1; i++) {
			Object obj = contentList1.get(i);
			if (toCheckObject(obj)) {
				elemCount1++;
				String guid = ((MethodElement) obj).getGuid();
				String name = ((MethodElement) obj).getName();
				if (! comparedElemMap.containsKey(guid)) {
					elementMap.put(guid, obj);
					if (useNameAs2ndId) {
						Object objInMap = elementNameMap.get(name);
						if (objInMap == null) {
							elementNameMap.put(name, obj);
						} else {
							List list = null;
							if (objInMap instanceof List) {
								list = (List) objInMap;
							} else {
								list = new ArrayList();
								list.add(objInMap);
							}
							list.add(obj);
						}
						
						List childList = null;
						if (obj instanceof MethodPackage) {
							childList = ((MethodPackage) obj).eContents();
						}
						if (childList != null && ! childList.isEmpty()) {
							MethodElement ch0 = (MethodElement) childList.get(0);
							elementChildMap.put(ch0.getGuid(), obj);
						}						
					}
					
					comparedElemMap.put(guid, obj);
				}
			}
		}
		int elemCount2 = 0;
		for (int i=0; i < sz2; i++) {
			Object obj = contentList2.get(i);
			if (toCheckObject(obj)) {
				elemCount2++;
			}
		}
		if (elemCount1 < elemCount2 || elemCount1 > elemCount2 && checkEqualSize) {
			String msg = "elemCount1 != elemCount2: " + elemCount1 + " != " + elemCount2;
			logDiff(msg, path, elem1, elem2);
			if (!greedy) {
				return;
			}
		}
				
		if (! (elem1 instanceof MethodLibrary)) {
			List properties = elem1.getInstanceProperties();
			for (int i = 0; i < properties.size(); i++) {
				EProperty ep = (EProperty) properties.get(i);
				EStructuralFeature feature = ep.getEStructuralFeature();
				if (skipFeatureSet.contains(feature.getName())) {
					continue;
				}
				Object val1 = "1 ... ?";
				Object val2 = "2 ... ?";
				try {
					val1 = elem1.eGet(feature);
					val2 = elem2.eGet(feature);
				} catch (Throwable e) {					
				}
				if (! featureValueEquals(val1, val2)) {
					if (feature.getName() != "guid" || !useNameAs2ndId) {
						String msg = "Diff values in feature: " + feature.getName();
						logDiff(msg, path, elem1, elem2, val1, val2);
						if (!greedy) {
							return;
						}
					}
				}
			}
		}
				
		//Compare for contained elements		
		for (int i=0; i < sz2; i++) {
			Object obj = contentList2.get(i);
			if (! toCheckObject(obj)) {
				continue;
			}
			
			MethodElement subElem2 = (MethodElement) obj;
			MethodElement subElem1 = (MethodElement) elementMap.get(subElem2.getGuid());
			if (useNameAs2ndId && subElem1 == null) {
				Object subObj1 = elementNameMap.get(subElem2.getName());
				if (subObj1 instanceof MethodElement) {
					subElem1 = (MethodElement)  subObj1;
				} else if (subObj1 instanceof List) {
					logWarning("subObj1 is list: " + subObj1.toString(), path, subElem1, subElem2);			
				}
				if (subElem1 == null && subElem2 instanceof MethodPackage) {
					List childList = ((MethodPackage) subElem2).eContents();
					if (childList != null && ! childList.isEmpty()) {
						MethodElement ch0 = (MethodElement) childList.get(0);
						subElem1 =  (MethodElement) elementChildMap.get(ch0.getGuid());
					}	
				}
			}
			if (subElem1 == null) {
				logDiff("subElem1 == null" , path, subElem1, subElem2);			
				if (!greedy) {
					return;
				}
				continue;
			}			
			compare(path, subElem1, subElem2, comparedElemMap);			
			if (diffCount > 0 && !greedy) {
				return;
			}
		}					
	}
	
	private boolean featureValueEquals(Object val1, Object val2) {
		if (val1 == null) {
			return val2 == null;
		}
		
		if (val2 == null) {
			return val1 == null;
		}
		
		if (val1 instanceof MethodElement) {
			if (! (val1 instanceof MethodElement)) {
				return false;
			}
			boolean b  = ((MethodElement) val1).getGuid().equals(((MethodElement) val1).getGuid());
			if (!b && useNameAs2ndId) {
				b = ((MethodElement) val1).getName().equals(((MethodElement) val1).getName());
			}
			return b;
		}				
		
		int option = 0; 	//0: not to compare list values
							//1:        compare list values
							//2:		compare size strictly
		if (option > 0 && val1 instanceof List) {			
			if (! (val2 instanceof List)) {
				return false;
			}
			List l1 = (List) val1;
			List l2 = (List) val2;
			if (l1.size() < l2.size()) {
				return false;
			}
			
			if (option > 1 && l1.size() > l2.size()) {		
				return false;
			}

			boolean me = l1.size() == 0 ? false : l1.get(0) instanceof MethodElement;
			if (me) {
				Map map = new HashMap();
				Map nameMap = useNameAs2ndId ? new HashMap() : null;
				for (int i=0; i<l1.size(); i++) {
					MethodElement e1 = (MethodElement) l1.get(i);
					map.put(e1.getGuid(), e1);
					if (useNameAs2ndId) {
						nameMap.put(e1.getName(), e1);
					}
				}
				for (int i=0; i<l2.size(); i++) {
					MethodElement e2 = (MethodElement) l2.get(i);
					boolean found = map.containsKey(e2.getGuid()) || 
							useNameAs2ndId && nameMap.containsKey(e2.getName());
					if (!found) {
						return false;
					}
				}					
			} else {
				for (int i=0; i<l1.size(); i++) {
					if (! (featureValueEquals(l1.get(i), l2.get(i))))  {
						return false;
					}
				}
			}
		}
				
		if (val1 instanceof String) {
			if (!(val2 instanceof String)) {
				return false;
			}
			String str1 = (String) val1;
			String str2 = (String) val2;
			if (skipRef) {
				str1 = replaceRefWithDummy(str1);
				str2 = replaceRefWithDummy(str2);
			}
			
			if (trim) {
				str1 = str1.trim();
				str2 = str2.trim();
				if (useNameAs2ndId && str1.length() > 250 && str2.length() > 250) {
					str1 = str1.substring(0, 250);
					str2 = str2.substring(0, 250);
				}
			}
			return str1.equals(str2);
		}
		
		return true;
	}
	
	private int incDiffCount() {
		return ++diffCount;
	}
	
	private String getDiffPrompt() {
		return "D_" + diffCount + "> ";	
	}
	
	private void logWarning(String msg, List path, MethodElement elem1, MethodElement elem2) {
		log(msg, path, elem1, elem2, false);
		trace("");
	}
	
	private void logDiff(String msg, List path, MethodElement elem1, MethodElement elem2) {
		log(msg, path, elem1, elem2, true);
		trace("");
	}
	
	private void log(String msg, List path, MethodElement elem1, MethodElement elem2, boolean diff) {
		if (diff) {
			incDiffCount();
		}
		String prompt = diff ? getDiffPrompt() : "Warning> ";
		trace(prompt + "path: " + pathToString(path));
		trace(prompt + "msg: " + msg);
		MethodElement elem0 = (MethodElement) path.get(path.size() - 1);
		if (elem0 != elem1) {
			trace(prompt + getElemString(elem0, "elem0"));
		}
		trace(prompt + getElemString(elem1, "elem1"));
		trace(prompt + getElemString(elem2, "elem2"));
	}
	
	private String getElemString(MethodElement elem, String label) {
		String str = label;
		if (elem == null) {
			str += " -> null";
		} else {		
			str += " -> type: " + getClassLastName(elem) + ", name: " + elem.getName() + ", guid: " + elem.getGuid();
		}
		return str;
	}
	
	private String getClassLastName(Object obj) {
		String str = obj.getClass().getName();
		int ix = str.lastIndexOf(".") + 1;
		return str.substring(ix);		
	}
	
	private void logDiff(String msg, List path, MethodElement elem1, MethodElement elem2, Object val1, Object val2) {
		log(msg, path, elem1, elem2, true);
		String prompt = getDiffPrompt();
		trace(prompt + "val1: " + val1);
		trace(prompt + "val2: " + val2);	
		trace("");
	}	
	
	private static String pathToString(List path) {
		StringBuffer buf = new StringBuffer();
		for (int i=0; i<path.size(); i++) {
			MethodElement elem = (MethodElement) path.get(i);
			if (elem instanceof MethodLibrary) {
				continue;
			}
			if (buf.length() > 0) {
				buf.append(":");
			}
			buf.append(elem.getName());
		}
		return buf.toString();
	}
	
	public int getDiffCount() {
		return diffCount;
	}
	
	public int getElemComparedCount() {
		return elemComparedCount;
	}
	
	private boolean toCheckObject(Object obj) {
		if (! (obj instanceof MethodElement)) {
			return false;
		}
		
		if (useNameAs2ndId && obj instanceof Diagram) {
			return false;
		}
		
		return true;
	}
		
	private String replaceRefWithDummy(String source) {		
		String ret = replaceRefWithDummy(source, ResourceScanner.p_src_ref, emptyString);
		ret = replaceRefWithDummy(ret, ResourceScanner.p_href_ref, emptyString);
		ret = replaceRefWithDummy(ret, emptyLine, emptyString);
		if (false && !source.equals(ret)) {
			trace("LD> source: " + source);
			trace("LD> ret:    " + ret);
		}
		return ret;
	}
	
	private String replaceRefWithDummy(String source, Pattern pattern, String replace) {
		StringBuffer sb = new StringBuffer();
		Matcher m = pattern.matcher(source);
	
		while (m.find()) {
			String text = m.group();						
			m.appendReplacement(sb, replace);	
		}

		m.appendTail(sb);
		return sb.toString();
	}
	
}
