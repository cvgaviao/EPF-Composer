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
package org.eclipse.epf.library.tests.exportimport;

import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.library.tester.impl.ExportImportTestImpl;

/**
 * JUnit tests for plugin export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public abstract class ExportImportTestMethodBase {
	private ExportImportTestImpl owner;
	
	public ExportImportTestMethodBase(ExportImportTestImpl owner) {
		this.owner = owner;
	}
	
	protected ExportImportTestImpl getOwner() {
		return owner;
	}	
	
	abstract public boolean execute(String testName);
	
}
