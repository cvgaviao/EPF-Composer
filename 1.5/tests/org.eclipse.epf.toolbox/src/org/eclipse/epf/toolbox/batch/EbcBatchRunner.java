package org.eclipse.epf.toolbox.batch;

import java.io.File;

import org.eclipse.epf.common.service.utils.CommandLineRunner;
import org.eclipse.epf.toolbox.ToolboxPlugin;

public class EbcBatchRunner extends CommandLineRunner {
	
	public boolean execute(String[] args) {
		
		if (true || localDebug) {
			System.out.println("LD> EpfBatchRunner.execute, args: "); //$NON-NLS-1$
			for (int i = 0; i < args.length; i++) {
				System.out.println("LD> args[" + i + "]: " + args[i]); //$NON-NLS-1$//$NON-NLS-2$
			}
		}		
		
		File inputFile = null;
		for (int i = 0; i < args.length; i++) {
			if (args[i].equalsIgnoreCase("-epfBatch")) { //$NON-NLS-1$
				if (i + 1 < args.length) {
					String path = args[i + 1];
					inputFile = new File(path);
					if (!inputFile.exists()) {						
						String msg = "Cannot fine the file: " + path; //$NON-NLS-1$
						ToolboxPlugin.getDefault().getLogger().logError(msg);
						return false;
					}
					break;
				}
			}
		}
		
		if (inputFile == null) {
			String msg = "Input file is missing"; //$NON-NLS-1$
			ToolboxPlugin.getDefault().getLogger().logError(msg);
			return false;
		}
		
		EbcBatchCommandMgr ebcMgr = new EbcBatchCommandMgr(inputFile);
		EbcExeReplies replies = ebcMgr.execute();
				
		return true;
	}	
	
}
