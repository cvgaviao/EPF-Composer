package org.eclipse.epf.library.util;

import java.util.List;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.ILibraryEditUtilProvider;
import org.eclipse.epf.library.edit.util.LibraryEditUtil;
import org.eclipse.epf.library.edit.util.MethodLibraryPropUtil;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.library.edit.validation.IValidationManager;
import org.eclipse.epf.library.validation.ValidationManager;
import org.eclipse.epf.persistence.MultiFileResourceSetImpl;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;

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
		boolean oldValue = MultiFileResourceSetImpl.REPORT_ERROR;
		if (oldValue) {
			MultiFileResourceSetImpl.REPORT_ERROR = false;
		}
		MethodElement ret = mgr == null ? null : mgr.getMethodElement(guid);
		if (oldValue) {
			MultiFileResourceSetImpl.REPORT_ERROR = true;
		}
		return ret;
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
				boolean isLinkedType = 	obj instanceof Task ||
										obj instanceof Role ||
										obj instanceof WorkProduct;
				if (!isLinkedType) {
					return false;
				}
				
				//Temporary only: need to add config as argument of the method
				MethodConfiguration config = LibraryService.getInstance().getCurrentMethodConfiguration();
				
				ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(config);
				
				MethodElement element = ProcessUtil.getAssociatedElement(desc);
				if (element == null) {
					return false;
				}
				EReference elemRef = LibraryEditUtil.getInstance().getLinkedElementFeature(ref);
				List<MethodElement> elementList = ConfigurationHelper.calc0nFeatureValue(element,
						elemRef, realizer);
								
				return elementList == null ? false : elementList.contains(obj);
			}
			
			Descriptor des = (Descriptor)obj;			
			if (ProcessUtil.getAssociatedElement(des) == null) {
				return false;
			}
			
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
	
	public MethodLibrary getCurrentMethodLibrary() {
		return LibraryService.getInstance().getCurrentMethodLibrary();
	}
	
	public String getPresentationName(MethodElement element,
			MethodConfiguration config) {
		return ConfigurationHelper.getPresentationName(element, config);
	}
	
	public IRealizationManager getRealizationManager(MethodConfiguration config) {
		return ConfigurationHelper.getDelegate().getRealizationManager(config);
	}
	
	public MethodElement getCalculatedElement(MethodElement element, MethodConfiguration config) {
		return  ConfigurationHelper.getCalculatedElement(element, config);
	}
	
	public boolean inConfig(MethodElement element, MethodConfiguration config) {
		return  ConfigurationHelper.inConfig(element, config);
	}
	
	public IValidationManager getValidationManager() {
		return ValidationManager.getInstance();
	}
	
	public List<MethodElement> calc0nFeatureValue(MethodElement element,
			EStructuralFeature feature, MethodConfiguration config) {
		ElementRealizer realizer = DefaultElementRealizer
		.newElementRealizer(config);
		return ConfigurationHelper.calc0nFeatureValue(element, feature, realizer);
	}
	
}
