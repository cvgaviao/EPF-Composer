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

import org.eclipse.epf.library.tester.iface.LibraryTester;
import org.eclipse.epf.library.tester.impl.LibraryTesterImpl;

/**
 * Class factory for LibraryTeser instances
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class LibraryTesterFactory {	
	
	public static LibraryTester newTester(boolean trace) {			
		return new LibraryTesterImpl(trace);
	}
			
}
