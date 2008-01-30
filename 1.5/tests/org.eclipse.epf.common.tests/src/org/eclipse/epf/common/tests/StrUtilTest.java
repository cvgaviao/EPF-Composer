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

import org.eclipse.epf.common.utils.StrUtil;

/**
 * JUnit tests for the org.eclipse.epf.common.utils.StrUtil classes.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class StrUtilTest extends TestCase {
	
	/**
	 * Creates a new instance.
	 */
	public StrUtilTest(String name) {
		super(name);
	}

	/**
	 * Tests the StrUtil.isNull() method.
	 */
	public void testIsNull() throws Exception {
	    assertTrue(StrUtil.isNull(null));
	    assertTrue(StrUtil.isNull(""));
	    assertFalse(StrUtil.isNull("Hello"));	    
	}
	
	/**
	 * Tests the StrUtil.isBlank() method.
	 */	
	public void testIsBlank() throws Exception {
	    assertTrue(StrUtil.isBlank(null));
	    assertTrue(StrUtil.isBlank(""));
	    assertFalse(StrUtil.isBlank("Hello"));	    
	}	
	
}
