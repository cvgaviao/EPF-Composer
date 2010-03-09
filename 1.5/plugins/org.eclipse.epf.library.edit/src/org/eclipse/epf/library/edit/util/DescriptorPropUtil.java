package org.eclipse.epf.library.edit.util;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class DescriptorPropUtil extends MethodElementPropUtil {
	
	private static final String infoSeperator = "/"; 							//$NON-NLS-1$

	public static final String DESCRIPTOR_SynFree = "descriptor_synFree"; 				//$NON-NLS-1$
	public static final String DESCRIPTOR_IsDynamic = "descriptor_isDynamic"; 			//$NON-NLS-1$
	public static final String DESCRIPTOR_Customization = "descriptor_customization"; 	//$NON-NLS-1$
	public static final String DESCRIPTOR_LocalUsingInfo = "descriptor_localUsingInfo";	//$NON-NLS-1$
	
	private static int nameReplace = 				1;		//0000000000000001
	private static int presentatioNameReplace = 	2;		//0000000000000010
	private static int briefDesReplace = 			4;		//0000000000000100
	
	private static DescriptorPropUtil descriptorPropUtil = new DescriptorPropUtil();
	public static DescriptorPropUtil getDesciptorPropUtil() {
		return descriptorPropUtil;
	}
	public static DescriptorPropUtil getDesciptorPropUtil(IActionManager actionManager) {
		return new DescriptorPropUtil(actionManager);
	}
	
	protected DescriptorPropUtil() {		
	}
	
	protected DescriptorPropUtil(IActionManager actionManager) {
		super(actionManager);
	}
	
	public boolean isSynFree(Descriptor d) {
		Boolean value = getBooleanValue(d, DESCRIPTOR_SynFree);
		return value == null ? false : value.booleanValue();
	}
	
	public void setSynFree(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_SynFree, value);
	}
	
	public boolean isDynamic(Descriptor d) {
		Boolean value = getBooleanValue(d, DESCRIPTOR_IsDynamic);
		return value == null ? false : value.booleanValue();
	}
	
	public void setDynamic(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_IsDynamic, value);
	}
	
	public boolean isNameRepalce(Descriptor d) {
		if (hasNoValue(d.getName())) {
			return false;
		}
		return getCustomization(d, nameReplace);
	}
	
	public void setNameRepalce(Descriptor d, boolean value) {
		setCustomization(d, nameReplace, value);
	}	
	
	public boolean isPresentationNameRepalce(Descriptor d) {
		if (hasNoValue(d.getPresentationName())) {
			return false;
		}
		return getCustomization(d, presentatioNameReplace);
	}
	
	public void setPresentationNameRepalce(Descriptor d, boolean value) {
		setCustomization(d, presentatioNameReplace, value);
	}
	
	public boolean isBriefDesRepalce(Descriptor d) {
		if (hasNoValue(d.getBriefDescription())) {
			return false;
		}
		return getCustomization(d, briefDesReplace);
	}
	
	public void setBriefDesRepalce(Descriptor d, boolean value) {
		setCustomization(d, briefDesReplace, value);
	}
	
	//Check if usedD is locally used by usingD
	public boolean localUse(Descriptor usedD, Descriptor usingD, EReference feature) {
		try {
			return localUse_(usedD, usingD, feature);			
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}		
		return false;
	}
	
	private boolean localUse_(Descriptor usedD, Descriptor usingD, EReference feature) {
		String value = getStringValue(usedD, DESCRIPTOR_LocalUsingInfo);
		if (value == null || value.length() == 0) {
			return false;
		}
		
		String[] infos = value.split(infoSeperator);
		if (infos == null || infos.length == 0) {
			return false;
		}
		
		int sz = infos.length / 2; 		
		for (int i = 0; i < sz; i++) {
			int i1 = i*2;
			int i2 = i1 + 1;
			String iGuid = infos[i1];
			String iFeature = infos[i2];
			if (iGuid.equals(usingD.getGuid()) && iFeature.endsWith(feature.getName())) {
				return true;
			} 
		}		
		return false;
		
	}
	
	public void addLocalUse(Descriptor usedD, Descriptor usingD, EReference feature) {
		try {
			addLocalUse_(usedD, usingD, feature);			
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
	}
	
	private void addLocalUse_(Descriptor usedD, Descriptor usingD, EReference feature) {
		String oldValue = getStringValue(usedD, DESCRIPTOR_LocalUsingInfo);
		String newValue = usingD.getGuid().concat(feature.getName());

		if (oldValue != null && oldValue.length() > 0) {			
			String[] infos = oldValue.split(infoSeperator); 
			
			int sz = infos.length / 2; 		
			for (int i = 0; i < sz; i++) {
				int i1 = i*2;
				int i2 = i1 + 1;
				String iGuid = infos[i1];
				String iFeature = infos[i2];
				if (iGuid.equals(usingD.getGuid()) && iFeature.equals(feature.getName())) {
					return;
				} 
			}
			
			newValue = oldValue.concat(infoSeperator).concat(newValue);
		}				
		setStringValue(usedD, DESCRIPTOR_LocalUsingInfo, newValue);
	}
	
	public void removeLocalUse(Descriptor usedD, Descriptor usingD, EReference feature) {
		try {
			removeLocalUse_(usedD, usingD, feature);			
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
	}
	
	private void removeLocalUse_(Descriptor usedD, Descriptor usingD, EReference feature) {
		String oldValue = getStringValue(usedD, DESCRIPTOR_LocalUsingInfo);
		if (oldValue == null || oldValue.length() == 0) {
			return;
		}
		boolean removed = false;
		String newValue = ""; //$NON-NLS-1$

		if (oldValue != null && oldValue.length() > 0) {			
			String[] infos = oldValue.split(infoSeperator); 
			
			int sz = infos.length / 2; 		
			for (int i = 0; i < sz; i++) {
				int i1 = i*2;
				int i2 = i1 + 1;
				String iGuid = infos[i1];
				String iFeature = infos[i2];
				if (iGuid.equals(usingD.getGuid()) && iFeature.equals(feature.getName())) {
					removed = true;		
				} else {
					if (newValue.length() > 0) {
						newValue = newValue.concat(infoSeperator);
					}
					newValue = newValue.concat(iGuid.concat(iFeature));
				}
			}

		}
		
		if (removed) {
			setStringValue(usedD, DESCRIPTOR_LocalUsingInfo, newValue);
		}

	}
	
	public boolean hasNoValue(String str) {
		return str == null || str.trim().length() == 0;
	}
		
	protected boolean getCustomization(Descriptor d, int maskBit) {
		Integer cusValue = getIntValue(d, DESCRIPTOR_Customization);
		int cus = cusValue == null ? 0 : cusValue.intValue();
		return (cus & maskBit) > 0;
	}
	
	protected void setCustomization(Descriptor d, int maskBit, boolean value) {
		Integer cusValue = getIntValue(d, DESCRIPTOR_Customization);
		int cus = cusValue == null ? 0 : cusValue.intValue();
		boolean oldValue = (cus & maskBit) > 0;
		if (oldValue == value) {
			return;
		}
		if (value) {
			cus |= maskBit;
		} else {
			cus &= maskBit;
		}
		setIntValue(d, DESCRIPTOR_Customization, cus);
	}	
	
	public MethodElement getLinkedElement(Descriptor des) {
		if (des instanceof TaskDescriptor) {
			return ((TaskDescriptor) des).getTask();
		}
		if (des instanceof RoleDescriptor) {
			return ((RoleDescriptor) des).getRole();
		}
		if (des instanceof WorkProductDescriptor) {
			return ((WorkProductDescriptor) des).getWorkProduct();
		}
		
		return null;
	}
	
	public boolean isValueReplaced(EStructuralFeature feature, Descriptor d) {
		UmaPackage up = UmaPackage.eINSTANCE;
		if (feature == up.getNamedElement_Name()) {
			return isNameRepalce(d);
		}
		if (feature == up.getMethodElement_PresentationName()) {
			return isPresentationNameRepalce(d);
		}
		if (feature == up.getMethodElement_BriefDescription()) {
			return isBriefDesRepalce(d);
		}
		
		return true;
	}
	
	
}
