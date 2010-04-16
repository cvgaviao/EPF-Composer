package org.eclipse.epf.library.util;

import java.util.List;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.ILibraryEditUtilProvider;
import org.eclipse.epf.library.edit.util.MethodLibraryPropUtil;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.UmaPackage;

public class LibraryEditUtilProvider implements ILibraryEditUtilProvider {
	private DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
	
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
		if (! (obj instanceof MethodElement) || !ref.isMany()) {
			return false;
		}
		EReference eRef = propUtil.getExcludeFeature(ref);
		if (eRef == null) {
			return false;
		}
		
		List<MethodElement> listValue = (List<MethodElement> )desc.eGet(eRef);
	    if (listValue == null) {
	    	return false;
	    }
	    listValue = ConfigurationHelper.getCalculatedElements(listValue, config);
	    
		return listValue.contains(obj);
	}

	public boolean isDynamic(Object obj, Descriptor desc, EReference ref) {
		if (ProcessUtil.isSynFree()) {
			if (!(obj instanceof Descriptor)) {// Excluded elements are not descriptors
				return true;
			}
			
			Descriptor des = (Descriptor)obj;			
//			if (! isCreatedByReference(des)) {
//				return false;
//			}
			
			if (! propUtil.localUse(des, desc, ref)) {
				return true;
			}
		}

		return false;
	}
	
	public boolean isGuidanceDynamic(Object obj, Descriptor desc,
			MethodConfiguration config) {
		if (ProcessUtil.isSynFree()) {
			EReference aRef = UmaPackage.eINSTANCE.getDescriptor_GuidanceAdditional();
			
			List<MethodElement> listValue = (List<MethodElement> )desc.eGet(aRef);
		    if (listValue == null) {
		    	return false;
		    }
		    listValue = ConfigurationHelper.getCalculatedElements(listValue, config);
		    
		    if (!listValue.contains(obj)) {
		    	return true;		    	
		    }
		}
		
		return false;
	}
	
}
