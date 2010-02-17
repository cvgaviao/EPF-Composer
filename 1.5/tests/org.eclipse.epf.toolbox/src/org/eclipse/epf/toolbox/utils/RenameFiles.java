package org.eclipse.epf.toolbox.utils;

import java.io.File;

public class RenameFiles {

	//Given strings
		
	private static final String lang = "_ar";
	private static final String pkg = ".nlBidi";
	private static final String fpkg = ".nlsBidi";
	private static final String rootPath = "E:/Root/a_Rmc7.5.0.1/Tvt/Returns/1stReturns/renamed/ARA/RmcTool";

	
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
		instance.execute();
	}	

}
