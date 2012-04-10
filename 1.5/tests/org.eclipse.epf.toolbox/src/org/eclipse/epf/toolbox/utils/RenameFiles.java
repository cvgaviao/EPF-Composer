package org.eclipse.epf.toolbox.utils;

import java.io.File;

public class RenameFiles {

	//Given strings
		
	private static String lang = "_ar";
	private static String pkg = ".nlBidi";
	private static String fpkg = ".nlsBidi";
	private static String rootPath = "E:/Root/a_Rmc7.5.0.1/Tvt/Returns/1stReturns/renamed/ARA/RmcTool";

	private final static boolean runAll = true;	
	private static String rootPathPrefix = "C:\\a_dev\\Rmc7.5.2\\TVT\\Returns\\1stReturns\\renamed\\";
	private static String lastFoler = "RmcTool";
	private static String langFolerList[] = {"DEU", "ESP", "FRA", "ITA", "JPN", "KOR", "PTB", "CHS", "CHT", "PLK", "RUS", "DAN", "ARA"};
	
	private static String langList[] = {"_de", "_es", "_fr", "_it", "_ja", "_ko", "_pt_BR", "_zh", "_zh_TW", "_pl", "_ru", "_da", "_ar"};
	private static String[] pkgList = null;
	private static String[] fpkgList = null;
	private static String[] rootPathList = null; 
	
	static {
		int sz = 13;
		pkgList = new String[sz];
		fpkgList = new String[sz];
		rootPathList =  new String[sz];
		for (int i = 0; i < sz; i ++) {
			if (i < 9) {
				pkgList[i] = ".nl1";
				fpkgList[i] = ".nls1";
			} else if (i < 11) {
				pkgList[i] = ".nl2";
				fpkgList[i] = ".nls2";
			} else if (i < 12) {
				pkgList[i] = ".nl2a";
				fpkgList[i] = ".nls2a";
			} else {
				pkgList[i] = ".nlBidi";
				fpkgList[i] = ".nlsBidi";	
			}
			rootPathList[i] = rootPathPrefix + langFolerList[i] + "\\" + lastFoler;
		}
	}
	
	private static final String[] props = {
		".properties",
		".htm",
		".html",
		".js",
	};
	
	private static final String epf = "org.eclipse.epf.";
	private static final String rmc = "com.ibm.rmc.";
	
	private static RenameFiles instance = new RenameFiles(rootPath);
	private File rootFolder;
	private int renamedFileCount = 0;
		
	public RenameFiles(String rootPath) {
		rootFolder = new File(rootPath);
	}
	
	public void execute() {
		File[] pluginFolders = rootFolder.listFiles();
		if (pluginFolders == null) {
			return;
		}
		
		System.out.println("LD> rename folders -> ");
		for (int i = 0; i < pluginFolders.length; i++) {
			File folderFile = pluginFolders[i];
			renameFolder(folderFile);
		}
		System.out.println("LD> rename folders <- \n");	
		
		pluginFolders = rootFolder.listFiles();
		renamedFileCount = 0;
		System.out.println("LD> rename files -> ");
		for (int i = 0; i < pluginFolders.length; i++) {
			File folderFile = pluginFolders[i];
			renameFiles(folderFile);
		}
		System.out.println("LD> rename files <- " + renamedFileCount);
		
	}
	
	private void renameFolder(File folderFile) {
		if (! folderFile.isDirectory()) {
			return;
		}
		String folderName = folderFile.getName();
		if ((folderName.startsWith(epf) || folderName.startsWith(rmc))
				&& !folderName.endsWith(pkg)) {
			boolean isFeature = folderName.endsWith(".feature");
			String lastStr = isFeature ? fpkg : pkg;
			
			File renamedFile = new File(folderFile.getAbsolutePath() + lastStr);

			folderFile.renameTo(renamedFile);
			System.out.println("LD> renamedFile: " + renamedFile);
		} else {
			System.out.println("LD> folderFile: " + folderFile);
		}
	}
	
	private void renameFiles(File folderFile) {
		if (! folderFile.isDirectory()) {
			return;
		}
		File[] children = folderFile.listFiles();
		if (children == null) {
			return;
		}
		for (int i = 0; i < children.length; i++) {
			File child = children[i];
			if (child.isDirectory()) {
				renameFiles(child);
			} else {
				String childName = child.getName();
				for (int j = 0; j < props.length; j++) {
					String prop = props[j];
					String newProp = lang + prop;
					if (childName.endsWith(prop) && !childName.endsWith(newProp)) {
						String oldPath = child.getAbsolutePath();
						int ix = oldPath.lastIndexOf(prop);
						String newPath = oldPath.substring(0, ix) + newProp;
						System.out.println("LD> oldPath: " + oldPath);
						System.out.println("LD> newPath: " + newPath + "\n");
						child.renameTo(new File(newPath));
						renamedFileCount++;
						break;
					}
				}
			}
		}
		
				
	}
	
	
	public static void main(String[] args) {
		if (runAll) {
			for (int i = 0; i < langList.length; i++) {
				lang = langList[i];
				pkg =  pkgList[i];
				fpkg = fpkgList[i];
				rootPath = rootPathList[i];
				instance = new RenameFiles(rootPath);
				instance.execute();
			}
		} else {
			instance.execute();
		}
	}	

}
