package org.eclipse.epf.library.util;

import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.util.ILibraryEditUtilProvider;
import org.eclipse.epf.library.edit.util.MethodLibraryPropUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;

public class LibraryEditUtilProvider implements ILibraryEditUtilProvider {

	public LibraryEditUtilProvider() {		
	}
	
	public boolean isSynFree() {
		MethodLibrary lib = LibraryService.getInstance().getCurrentMethodLibrary();
		if (lib != null) {
			return MethodLibraryPropUtil.getMethodLibraryPropUtil().isSynFree(lib);
		}
		
		return true;
	}

	public MethodElement getMethodElement(String guid) {
		ILibraryManager mgr = LibraryService.getInstance().getCurrentLibraryManager();
		return mgr == null ? null : mgr.getMethodElement(guid);
	}
	
}
