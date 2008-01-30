package org.eclipse.epf.library.tester.iface;

import java.io.File;

public interface ITestFolderSetup {

	/**
	 * Return top test root source folder file
	 */
	File getTopTestRootSource();
	
	/**
	 * Return test home folder file
	 */
	File getTestHome();	
	
}
