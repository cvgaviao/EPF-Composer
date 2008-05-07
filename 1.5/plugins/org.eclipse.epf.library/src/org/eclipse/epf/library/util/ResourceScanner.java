//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.util;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.epf.common.utils.FileUtil;
import org.eclipse.epf.library.ILibraryResourceManager;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.uma.MethodElement;

/**
 * Utility class to scan and copy resources from a plugin to another plugin.
 * @author Weiping Lu
 * @since 1.5
 *
 */
public class ResourceScanner {

	public static final Pattern p_src_ref = Pattern.compile(
			"src\\s*=\\s*\"(.*?)\"", Pattern.CASE_INSENSITIVE | Pattern.DOTALL); //$NON-NLS-1$
	public static final Pattern p_href_ref = Pattern
			.compile(
					"href\\s*=\\s*\"(.*?)\"", Pattern.CASE_INSENSITIVE | Pattern.DOTALL); //$NON-NLS-1$

	private File srcPluginRoot;
	private File tgtPluginRoot;
	private File srcPluginRootParent;
	private File tgtPluginRootParent;
	
	private Map<File, File> fileMap = new LinkedHashMap<File, File>();

	/**
	 * Creates a new instance.
	 */
	public ResourceScanner(File srcPluginRoot, File tgtPluginRoot) {
		this.srcPluginRoot = srcPluginRoot;
		this.tgtPluginRoot = tgtPluginRoot;
		srcPluginRootParent = srcPluginRoot.getParentFile();
		tgtPluginRootParent = tgtPluginRoot.getParentFile();
	}

	public void scan(MethodElement srcElement, MethodElement tgtElement, String source) {
		ILibraryResourceManager srcResMgr = ResourceHelper.getResourceMgr(srcElement);
		String srcPath = ResourceHelper.getElementPath(srcElement);
		
		ILibraryResourceManager tgtResMgr = ResourceHelper.getResourceMgr(tgtElement);
		String tgtPath = ResourceHelper.getElementPath(tgtElement);

		File srcFolder = new File(srcPluginRootParent, srcPath);
		File tgtFolder = new File(tgtPluginRootParent, tgtPath);
		
		try {
			// process images and other src resources
			Matcher m = p_src_ref.matcher(source);
			while (m.find()) {
				String url = m.group(1);
				registerFileCopy(srcFolder, tgtFolder, url);
			}

			// process hrefs
			m = p_href_ref.matcher(source);

			while (m.find()) {
				String url = m.group(1);
				registerFileCopy(srcFolder, tgtFolder, url);
			}
		} catch (Exception ex) {
			LibraryPlugin.getDefault().getLogger().logError(ex);
		}
	}
	
	public void registerFileCopy(File srcFolder, File tgtFolder, String url) {
		if (url == null) {
			return;
		}

		int index = url.indexOf("#"); //$NON-NLS-1$
		if (index >= 0) {
			url = url.substring(0, index);
		}

		index = url.indexOf("?"); //$NON-NLS-1$
		if (index >= 0) {
			url = url.substring(0, index);
		}

		if (url.trim().length() == 0) {
			return;
		}

		// the url is relative to the owner element
		// need to convert to the path relative to the library root
		File srcFile = null;
		File tgtFile = null;
		try {
			if (srcFile.isFile() && srcFile.exists()) {
				srcFile = srcFile.getCanonicalFile();
				tgtFile = tgtFile.getCanonicalFile();	
				fileMap.put(srcFile, tgtFile);				
			}
		} catch (IOException e) {
			// Log the error and proceed. TODO
			e.printStackTrace();
		}

	}


	/**
	 * copy all the files to the destination
	 */
	public void copyFiles() {
		for (Iterator it = fileMap.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry entry = (Map.Entry)it.next();
			File srcFile = (File) entry.getKey();
			File tgtFile = (File) entry.getValue();
			tgtFile = getFinalTargetFile(srcFile, tgtFile);
			if (tgtFile != null) {
				FileUtil.copyFile(srcFile, tgtFile);
			}
		}	

	}
	
	private File getFinalTargetFile(File srcFile, File tgtFile) {
		if (tgtFile.exists()) {
			boolean toCopy = (tgtFile.lastModified() != srcFile.lastModified())
					|| (tgtFile.length() != srcFile.length());
			return toCopy ? getRenamedTargetFile(tgtFile) : null;
		}
		
		return tgtFile;
	}
	
	private File getRenamedTargetFile(File tgtFile) {
		String dot = ".";	//$NON-NLS-1$
		File parentFile = tgtFile.getParentFile();
		String name = tgtFile.getName();
		String name1 = name;
		String name2 = "";	//$NON-NLS-1$
		
		int ix = name.lastIndexOf(dot);
		int len = name.length();

		boolean addDot = false;
		if (ix > 0 && ix < len) {
			name1 = name.substring(0, ix);
			name2 = name.substring(ix, len);
			addDot = true;
		}
		
		File file = tgtFile;
		String u = "_";	//$NON-NLS-1$
		int i = 1;
		while (file.exists()) {
			String newFileName = name1 + u + i;
			if (addDot) {
				newFileName += dot + name2;
			}
			file = new File(parentFile, newFileName);			
			i++;
		}
		return file;
	}


}
