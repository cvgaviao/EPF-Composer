package org.eclipse.epf.toolbox.utils;

import java.io.File;

import org.eclipse.epf.common.utils.FileUtil;

public class EditFiles {

	private static final String rootPath = "E:\\Root\\a_Rmc7.5.1\\TVT\\RMC7.5.1-1stDropWorkingDir\\Rmc7.5.1-1stDrop-Draft-1\\RmcTool";
	
	private static EditFiles instance = new EditFiles(rootPath);
	private File rootFolder;
	private int editFileCount = 0;
	
	private String insertedLine = "# NLS_ENCODING=UNICODE\n";
		
	public EditFiles(String rootPath) {
		rootFolder = new File(rootPath);
	}
	
	public void execute() {
		File[] pluginFolders = rootFolder.listFiles();
		if (pluginFolders == null) {
			return;
		}
			
		pluginFolders = rootFolder.listFiles();
		editFileCount = 0;
		System.out.println("LD> edited files -> ");
		for (int i = 0; i < pluginFolders.length; i++) {
			File folderFile = pluginFolders[i];
			editFiles(folderFile);
		}
		System.out.println("LD> edited files <- " + editFileCount);
		
	}
	
	private String getEditString(File file) {
		if (!file.getName().endsWith("properties")) {
			return null;
		}		
		
		String text = null;
		try {
			text = FileUtil.readFile(file, FileUtil.ENCODING_UTF_8)
					.toString();
			if (text == null || text.length() == 0) {
				return null;
			}
			int ix = text.indexOf("# NLS_ENCODING=");
			if (ix >= 0) {
				int ix1 = Math.min(ix + 22, text.length());
				System.out.println("LD> encoding: " + text.substring(ix, ix1));
				return null;
			}					
			
		} catch (Exception e) {
			e.printStackTrace();
		}

		return text;
	}
	
	private void editFiles(File folderFile) {
		if (!folderFile.isDirectory()) {
			return;
		}
		File[] children = folderFile.listFiles();
		if (children == null) {
			return;
		}
		for (int i = 0; i < children.length; i++) {
			File child = children[i];
			if (child.isDirectory()) {
				editFiles(child);
			} else {
				String text = getEditString(child);
				if (text != null) {
					text = insertedLine + text;
					try {
						FileUtil.writeFile(child.getAbsolutePath(), text);
						editFileCount++;
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
		}

	}	
	
	public static void main(String[] args) {
		instance.execute();
	}	

}
