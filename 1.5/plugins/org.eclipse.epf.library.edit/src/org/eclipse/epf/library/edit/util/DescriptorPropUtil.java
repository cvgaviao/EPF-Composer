package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.uma.Descriptor;

public class DescriptorPropUtil extends MethodElementPropUtil {
	
	private static final String guidSeperator = "/"; 							//$NON-NLS-1$

	public static final String DESCRIPTOR_Syn_Free = "syn_free"; 				//$NON-NLS-1$
	public static final String DESCRIPTOR_Is_Dynamic = "is_dynamic"; 			//$NON-NLS-1$
	public static final String DESCRIPTOR_Customization1 = "customization1"; 	//$NON-NLS-1$
	public static final String DESCRIPTOR_LocalUsingGuids = "LocalUsingGuids";	//$NON-NLS-1$
	
	private static int nameReplace = 				1;		//0000000000000001
	private static int presentatioNameReplace = 	2;		//0000000000000010
	private static int briefDesReplace = 			4;		//0000000000000100
	private static int mainDesReplace = 			8;		//0000000000001000
	private static int mainDesAppend = 				16;		//0000000000010000
	
	private static DescriptorPropUtil descriptorPropUtil = new DescriptorPropUtil();
	public static DescriptorPropUtil getDesciptorPropUtil() {
		return descriptorPropUtil;
	}
	
	protected DescriptorPropUtil() {		
	}
	
	public boolean isSynFree(Descriptor d) {
		Boolean value = getBooleanValue(d, DESCRIPTOR_Syn_Free);
		return value == null ? false : value.booleanValue();
	}
	
	public void setSynFree(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_Syn_Free, value);
	}
	
	public boolean isDynamic(Descriptor d) {
		Boolean value = getBooleanValue(d, DESCRIPTOR_Is_Dynamic);
		return value == null ? false : value.booleanValue();
	}
	
	public void setDynamic(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_Is_Dynamic, value);
	}
	
	public boolean isNameRepalce(Descriptor d) {
		return getCustomization1(d, nameReplace);
	}
	
	public void setNameRepalce(Descriptor d, boolean value) {
		setCustomization1(d, nameReplace, value);
	}	
	
	public boolean isPresentationNameRepalce(Descriptor d) {
		if (IRealizationManager.test) {
			return true;
		}
		return getCustomization1(d, presentatioNameReplace);
	}
	
	public void setPresentationNameRepalce(Descriptor d, boolean value) {
		setCustomization1(d, presentatioNameReplace, value);
	}
	
	public boolean isBriefDesReplaceRepalce(Descriptor d) {
		if (IRealizationManager.test) {
			return true;
		}
		return getCustomization1(d, briefDesReplace);
	}
	
	public void setBriefDesReplaceRepalce(Descriptor d, boolean value) {
		setCustomization1(d, briefDesReplace, value);
	}
	
	public boolean isMainDesReplace(Descriptor d) {
		return getCustomization1(d, mainDesReplace);
	}
	
	public void setMainDesReplace(Descriptor d, boolean value) {
		setCustomization1(d, mainDesReplace, value);
	}
	
	public boolean isMainDesAppend(Descriptor d) {
		return getCustomization1(d, mainDesAppend);
	}
	
	public void setMainDesAppend(Descriptor d, boolean value) {
		setCustomization1(d, mainDesAppend, value);
	}
	
	public boolean staticUse(Descriptor usedD, Descriptor usingD) {
		String value = getStringValue(usedD, DESCRIPTOR_LocalUsingGuids);
		if (value == null || value.length() == 0) {
			return false;
		}
		
		String[] guids = value.split(guidSeperator);
		if (guids == null || guids.length == 0) {
			return false;
		}
		
		for (String guid : guids) {
			if (guid.equals(usingD.getGuid())) {
				return true;
			} 
		}
		
		return false;
	}
	
	
	public void addLocalUse(Descriptor usedD, Descriptor usingD) {
		String oldValue = getStringValue(usedD, DESCRIPTOR_LocalUsingGuids);
		String newValue = usingD.getGuid();
		if (oldValue != null && oldValue.length() > 0) {
			String[] guids = oldValue.split(guidSeperator); 
			if (guids != null )
			for (String guid : guids) {
				if (guid.equals(usingD.getGuid())) {
					return;
				}
			}
			newValue = oldValue.concat(guidSeperator).concat(usingD.getGuid());
		}
		
		setStringValue(usedD, DESCRIPTOR_LocalUsingGuids, newValue);
	}
	
	public void removeLocalUse(Descriptor usedD, Descriptor usingD) {
		String oldValue = getStringValue(usedD, DESCRIPTOR_LocalUsingGuids);
		if (oldValue == null || oldValue.length() == 0) {
			return;
		}
		boolean removed = false;
		String newValue = ""; //$NON-NLS-1$
		String[] guids = oldValue.split(guidSeperator);
		for (String guid : guids) {
			if (guid.equals(usingD.getGuid())) {
				removed = true;				
			} else {
				if (newValue.length() > 0) {
					newValue = newValue.concat(guidSeperator);
				}
				newValue = newValue.concat(guid);
			}
		}

		if (removed) {
			setStringValue(usedD, DESCRIPTOR_LocalUsingGuids, newValue);
		}

	}
		
	protected boolean getCustomization1(Descriptor d, int maskBit) {
		int cus = getIntValue(d, DESCRIPTOR_Customization1);
		return (cus & maskBit) > 0;
	}
	
	protected void setCustomization1(Descriptor d, int maskBit, boolean value) {
		int cus = getIntValue(d, DESCRIPTOR_Customization1);
		boolean oldValue = (cus & maskBit) > 0;
		if (oldValue == value) {
			return;
		}
		if (value) {
			cus |= maskBit;
		} else {
			cus &= maskBit;
		}
		setIntValue(d, DESCRIPTOR_Customization1, cus);
	}	
	
}
