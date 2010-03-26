package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.uma.MethodElement;

public interface ILibraryEditUtilProvider {

	boolean isSynFree();
	MethodElement getMethodElement(String guid);
	
}
