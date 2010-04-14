package org.eclipse.epf.library.util;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.ILibraryEditUtilProvider;
import org.eclipse.epf.library.edit.util.MethodLibraryPropUtil;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;

public class LibraryEditUtilProvider implements ILibraryEditUtilProvider {

	public LibraryEditUtilProvider() {		
	}

	public boolean isSynFree() {
		MethodLibrary lib = LibraryService.getInstance()
				.getCurrentMethodLibrary();
		if (lib != null) {
			return MethodLibraryPropUtil.getMethodLibraryPropUtil().isSynFree(
					lib);
		}

		return true;
	}

	public MethodElement getMethodElement(String guid) {
		ILibraryManager mgr = LibraryService.getInstance()
				.getCurrentLibraryManager();
		return mgr == null ? null : mgr.getMethodElement(guid);
	}

	public boolean isDynamicAndExclude(Object obj, Descriptor desc,
			EReference ref, MethodConfiguration config) {
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();

		boolean b = propUtil.isDynamicAndExclude(obj, desc, ref);

		return b;
	}

	public boolean isDynamic(Object obj, Descriptor desc, EReference ref,
			MethodConfiguration config) {
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();

		boolean b = propUtil.isDynamic(obj, desc, ref);

		return b;
	}
	
}
