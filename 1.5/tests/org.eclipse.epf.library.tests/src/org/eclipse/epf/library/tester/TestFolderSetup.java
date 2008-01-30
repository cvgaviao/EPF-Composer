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

import org.eclipse.epf.library.tester.iface.ITestFolderSetup;
import org.eclipse.epf.library.tests.TestsPlugin;

/**
 * This class sets the top folder locations for LibraryTester 
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TestFolderSetup implements ITestFolderSetup {
	
	private static final File topTestRootSource = new File (TestsPlugin.getDefault().getInstallPath() +  
			"Libraries" + File.separator + "1.0.0" + File.separator) ;
		
	private static final File testHome = new File (System.getProperty("user.home") + File.separator //$NON-NLS-2$
			+ "EPF" + File.separator + "test" + File.separator);
	
	/**
	 * Return top test root source folder file
	 */
	public File getTopTestRootSource() {
		return topTestRootSource;
	}
	
	/**
	 * Return test home folder file
	 */
	public File getTestHome() {
		return testHome;
	}	

}
