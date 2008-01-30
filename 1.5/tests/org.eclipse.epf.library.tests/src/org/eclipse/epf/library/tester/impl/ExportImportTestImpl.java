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

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.tests.TestsPlugin;

import com.ibm.icu.text.SimpleDateFormat;
import com.ibm.icu.util.Calendar;

/**
 * JUnit tests for plugin export/import
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public abstract class ExportImportTestImpl extends LibraryJunitTestImpl {
	
	private static final String sourceFolderName = "ExportImport";
	private static  File testRootSource;

	private static Map testRootWorkingMap = new HashMap();
	
	private File defaultTestDir; 
	
	public ExportImportTestImpl(String name) {
		super(name);
	}
	
	protected synchronized File getTestRootSource() {
		if (testRootSource == null) {
			String path = getService().getTopTestRootSource().getAbsolutePath();
			path += File.separator + sourceFolderName +  File.separator;
			testRootSource = new File(path);
		}
		return testRootSource;
	}
	
	protected synchronized File getTestRootWorking() {
		File testRootWorking = (File) testRootWorkingMap.get(getClass());
		if (testRootWorking == null) {
			Class cls = getClass();
			String packageName = cls.getPackage().getName();
			
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss:SSS"); //$NON-NLS-1$
			String dtStr = sdf.format(new Date());
			dtStr = dtStr.replace(' ', '_');
			dtStr = dtStr.replace(':', '-');
			
			String path = getService().getTestGroup().getAbsolutePath();
			path += File.separator + cls.getName().substring(packageName.length() + 1);
			path += "_" + dtStr + File.separator;
			testRootWorking = new File(path);
			testRootWorkingMap.put(getClass(), testRootWorking);
		}
		return testRootWorking;
	}
	
	protected File getDefaultTestDir() {
		if (defaultTestDir == null) {
			String path = getTestRootWorking().getAbsolutePath() + File.separator + getName(); 
			defaultTestDir = new File(path);
		}
		
		return defaultTestDir;
	}
		
}
