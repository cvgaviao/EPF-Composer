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
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
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
	private Set<File> tgtFileSet = new HashSet<File>();

	/**
	 * Creates a new instance.
	 */
	public ResourceScanner(File srcPluginRoot, File tgtPluginRoot) {
		this.srcPluginRoot = srcPluginRoot;
		this.tgtPluginRoot = tgtPluginRoot;
		srcPluginRootParent = srcPluginRoot.getParentFile();
		tgtPluginRootParent = tgtPluginRoot.getParentFile();
	}

	public String scan(MethodElement srcElement, MethodElement tgtElement, String source) {
		ILibraryResourceManager srcResMgr = ResourceHelper.getResourceMgr(srcElement);
		String srcPath = ResourceHelper.getElementPath(srcElement);
		
		ILibraryResourceManager tgtResMgr = ResourceHelper.getResourceMgr(tgtElement);
		String tgtPath = ResourceHelper.getElementPath(tgtElement);

		File srcFolder = new File(srcPluginRootParent, srcPath);
		File tgtFolder = new File(tgtPluginRootParent, tgtPath);
		
		StringBuffer sb = new StringBuffer();
		try {
			// process images and other src resources
			Matcher m = p_src_ref.matcher(source);
			while (m.find()) {
				String text = m.group();
				String url = m.group(1);
				String tgtUrl = registerFileCopy(srcFolder, tgtFolder, url);
				String replaceText = text.replace(url, tgtUrl);
				m.appendReplacement(sb, Matcher.quoteReplacement(replaceText));	
			}
			m.appendTail(sb);

			// process hrefs
			m = p_href_ref.matcher(sb.toString());
			while (m.find()) {
				String text = m.group();
				String url = m.group(1);
				String tgtUrl = registerFileCopy(srcFolder, tgtFolder, url);
				String replaceText = text.replace(url, tgtUrl);
				m.appendReplacement(sb, Matcher.quoteReplacement(replaceText));	
			}
			m.appendTail(sb);
			
		} catch (Exception ex) {
			LibraryPlugin.getDefault().getLogger().logError(ex);
		}
		
		return null;
	}
	
	/**
	 * @param srcFolder
	 * @param tgtFolder
	 * @param url
	 * @return tgtUrl
	 */
	public String registerFileCopy(File srcFolder, File tgtFolder, String srcUrl) {
		if (srcUrl == null) {
			return srcUrl;
		}

		String tgtUrl = srcUrl;
		int index = tgtUrl.indexOf("#"); //$NON-NLS-1$
		if (index >= 0) {
			tgtUrl = tgtUrl.substring(0, index);
		}

		index = tgtUrl.indexOf("?"); //$NON-NLS-1$
		if (index >= 0) {
			tgtUrl = tgtUrl.substring(0, index);
		}

		if (tgtUrl.trim().length() == 0) {
			return srcUrl;
		}

		try {
			File srcFile = new File(srcFolder, srcUrl);
			File tgtFile = new File(tgtFolder, tgtUrl);
			
			if (srcFile.isFile() && srcFile.exists()) {
				if (tgtFile.exists()) {
					if (tgtFile.lastModified() == srcFile.lastModified()
							&& tgtFile.length() == srcFile.length()) {
						return tgtUrl;
					}
				}
				tgtUrl = this.getTargetUrl(srcFile, tgtFolder, tgtUrl);
				tgtFile = new File(tgtFolder, tgtUrl);;
				
				srcFile = srcFile.getCanonicalFile();
				tgtFile = tgtFile.getCanonicalFile();				
				fileMap.put(srcFile, tgtFile);				
			}
		} catch (IOException e) {
			LibraryPlugin.getDefault().getLogger().logError(e);
		}
		
		return tgtUrl;
	}


	/**
	 * copy all the files to the destination
	 */
	public void copyFiles() {
		for (Iterator it = fileMap.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry entry = (Map.Entry)it.next();
			File srcFile = (File) entry.getKey();
			File tgtFile = (File) entry.getValue();
			FileUtil.copyFile(srcFile, tgtFile);
		}	
	}
	
	private String getTargetUrl(File srcFile, File tgtFolder, String tgtUrl0) {
		String dot = ".";	//$NON-NLS-1$
		String url1 = tgtUrl0;
		String url2 = "";	//$NON-NLS-1$
		
		int ix = tgtUrl0.lastIndexOf(dot);
		int len = tgtUrl0.length();

		boolean addDot = false;
		if (ix > 0 && ix < len) {
			url1 = tgtUrl0.substring(0, ix);
			url2 = tgtUrl0.substring(ix, len);
			addDot = true;
		}
		
		String tgtUrl = tgtUrl0;
		File tgtFile = new File(tgtFolder, tgtUrl);
		String u = "_";	//$NON-NLS-1$
		int i = 1;
		while (tgtFile.exists() || tgtFileSet.contains(tgtFile)) {
			tgtUrl = url1 + u + i;
			if (addDot) {
				tgtUrl += dot + url2;
			}
			tgtFile = new File(tgtFolder, tgtUrl);			
			i++;
		}
		
		try {
			tgtFile = tgtFile.getCanonicalFile();
		} catch (Exception e) {
			LibraryPlugin.getDefault().getLogger().logError(e);
		}
		
		tgtFileSet.add(tgtFile);		
		return tgtUrl;
	}

}
