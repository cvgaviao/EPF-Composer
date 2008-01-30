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
package org.eclipse.epf.richtext.tests;

import junit.framework.TestCase;

import org.eclipse.epf.common.utils.StrUtil;

/**
 * JUnit tests for the org.eclipse.epf.richtext.RichText classes.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class RichTextTest extends TestCase {
	
	/**
	 * Creates a new instance.
	 */
	public RichTextTest(String name) {
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
	
}
