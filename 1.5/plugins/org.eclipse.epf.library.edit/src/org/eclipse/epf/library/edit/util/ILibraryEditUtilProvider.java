package org.eclipse.epf.library.edit.util;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;

public interface ILibraryEditUtilProvider {

	boolean isSynFree();

	MethodElement getMethodElement(String guid);

	boolean isDynamicAndExclude(Object obj, Descriptor desc, EReference ref,
			MethodConfiguration config);

	boolean isDynamic(Object obj, Descriptor desc, EReference ref);
	
	boolean isGuidanceDynamic(Object obj, Descriptor desc,
			MethodConfiguration config);

	MethodLibrary getCurrentMethodLibrary();
	
}
