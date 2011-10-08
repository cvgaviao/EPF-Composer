package org.eclipse.epf.toolbox.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.common.utils.FileUtil;

public class CopyPIIFiles {

	private static CopyPIIFiles instance = new CopyPIIFiles();
	
	private static boolean localDebug = true;
	
	private static final String[] startWithFiles = {
		"splash.",
	};
	
	private static final String[] types = {
		".properties",
		".htm",
		".html",
		".js",
	};
	
	private static final String[] excludeFiles = {
		"build.properties",
		"package.html",
	};
	
	private static final String[] pathNames = {
		"com.ibm.rmc",
		"org.eclipse.epf",
	};
	
	private static final String[] excludeFolders = {
		"bin",
		"CVS",
		"nl",
	};
	
	private static Set<String> typeSet; 
	private static Set<String> excludeFolderSet; 
	private static Set<String> excludeFileSet;
	
	
	static {
		typeSet = new HashSet<String>();
		for (String type : types) {
			typeSet.add(type);
		}
		
		excludeFolderSet = new HashSet<String>();
		for (String folder : excludeFolders) {
			excludeFolderSet.add(folder);
		}
		
		excludeFileSet = new HashSet<String>();
		for (String file : excludeFiles) {
			excludeFileSet.add(file);
		}
	}
	
	//Hard coded parameters
//	private String sourceRootFolderStr = "C:\\Documents and Settings\\wlu\\Desktop\\Dev\\aa_devTemp\\Rmc75SnapshortOn_04022009\\Rmc75SnapshortOn_04022009";
//	private String targetRootFolderStr = "C:\\Documents and Settings\\wlu\\Desktop\\Dev\\aa_devTemp\\Rmc75SnapshortOn_04022009\\RmcPIIFiles";	
	private String sourceRootFolderStr = "C:\\ec\\ec-Rmc7.5.1\\rmc-7.5.1.2-1007-2011";
	private String targetRootFolderStr = "E:\\Root\\a_Rmc7.5.1.2\\TVT\\PIIFiles\\Rmc7.5.1.2-1007-2011";	

	private File sourceRootFolder;
	private File targetRootFolder;
	
	private Set<File> nonProcessedFolderSet;
	private Set<File> nonProcessedFileSet;
	
	public CopyPIIFiles() {
		sourceRootFolder = new File(sourceRootFolderStr);
		targetRootFolder = new File(targetRootFolderStr);	
	}
	
	public static void main(String[] args) {
		instance.execute();
	}	
	
	public void execute() {
		nonProcessedFolderSet = new HashSet<File>();
		nonProcessedFileSet = new HashSet<File>();
		if (targetRootFolder.exists()) {
			FileUtil.deleteAllFiles(targetRootFolder.getAbsolutePath());
		} else {
			targetRootFolder.mkdir();
		}
		copy(sourceRootFolder, targetRootFolder);
		
		if (localDebug) {
			System.out.println("LD> folders not processed: ");
			for (File file : nonProcessedFolderSet) {
				System.out.println("LD> " + file.getAbsolutePath().substring(sourceRootFolderStr.length()));
			}
			System.out.println("");
			
			System.out.println("LD> files not processed: ");
			for (File file : nonProcessedFileSet) {
				System.out.println("LD> " + file.getAbsolutePath().substring(sourceRootFolderStr.length()));
			}
			System.out.println("");
		}
	}
	
	private boolean copy(File srcFolder, File tgtFolder) {
		
		File[] files = srcFolder.listFiles();
				
		List<File> subDirs = new ArrayList<File>();
		int ccCount = 0;
		List<String> copiedFileNames = new ArrayList<String>();
		for (File file : files) {
			if (file.isDirectory()) {
				if (toProcessFolder(file)) {
					subDirs.add(file);
				}
			} else {
				if (toCopyFile(file)) {
					ccCount++;
					File tgtFile = new File(tgtFolder, file.getName());
					FileUtil.copyFile(file, tgtFile);
					copiedFileNames.add(file.getName());
				}
			}
		}
		
		if (localDebug && ccCount > 0) {
			System.out.println("LD> Copied: " + ccCount + ", " + srcFolder.getAbsolutePath().substring(
							sourceRootFolderStr.length()));
			for (String fileName : copiedFileNames) {
				System.out.println("LD> Copied: " + fileName);
			}
			System.out.println("");
		}
		
		for (File subDir : subDirs) {
			File tgtSubDir = new File(tgtFolder, subDir.getName());
			tgtSubDir.mkdir();
			if (! copy(subDir, tgtSubDir)) {
				tgtSubDir.delete();
			}
		}		
		return ccCount > 0;		
	}
	
	private boolean toCopyFile(File file) {
		String fileName = file.getName();
		if (excludeFileSet.contains(fileName)) {
			nonProcessedFileSet.add(file);
			return false;
		}		
		
		for (String str : startWithFiles) {
			if (fileName.startsWith(str)) {
				return true;
			}
		}
		
		int ix = fileName.lastIndexOf(".");
		if (ix < 0) {
			return false;
		}
		String ext = fileName.substring(ix);
		return typeSet.contains(ext);
	}
	
	private boolean toProcessFolder(File folder) {
		boolean ret = toProcessFolder_(folder);
		if (!ret) {
			nonProcessedFolderSet.add(folder);
		}		
		return ret;
	}
	
	private boolean toProcessFolder_(File folder) {
		if (excludeFolderSet.contains(folder.getName())) {
			return false;
		}		
		String filePath = folder.getAbsolutePath();
		
		boolean containingName = false;		
		for (String str : pathNames) {
			if (filePath.indexOf(str) >= 0) {
				containingName = true;
				break;
			}
		}
		
		return containingName;

	}
	
}
