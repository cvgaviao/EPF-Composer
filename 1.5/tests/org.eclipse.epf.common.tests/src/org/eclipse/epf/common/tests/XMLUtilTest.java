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
package org.eclipse.epf.common.tests;

import junit.framework.TestCase;

import org.eclipse.epf.common.utils.XMLUtil;

/**
 * JUnit tests for org.eclipse.epf.common.utils.XMLUtil.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class XMLUtilTest extends TestCase {
	
	/**
	 * Creates a new instance.
	 */
	public XMLUtilTest(String name) {
		super(name);
	}
	
	/**
	 * Tests the XMLUtil.escapeAttr() method.
	 */	
	public void testEscapeAttr() throws Exception {
	    String result = XMLUtil.escape("<b>Hello</b>");
	    assertTrue(result.equals("&lt;b&gt;Hello&lt;/b&gt;"));
	}

}
