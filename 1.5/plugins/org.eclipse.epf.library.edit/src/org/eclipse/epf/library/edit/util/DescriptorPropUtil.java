package org.eclipse.epf.library.edit.util;

import org.eclipse.epf.uma.Descriptor;

public class DescriptorPropUtil extends MethodElementPropUtil {

	public static final String DESCRIPTOR_Syn_Free = "syn_free"; 			//$NON-NLS-1$
	public static final String DESCRIPTOR_Customization1 = "customization1"; 	//$NON-NLS-1$
	
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
		return getBooleanValue(d, DESCRIPTOR_Syn_Free);
	}
	
	public void setSynFree(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_Syn_Free, value);
	}
	
	public boolean isNameRepalce(Descriptor d) {
		return getCustomization1(d, nameReplace);
	}
	
	public void setNameRepalce(Descriptor d, boolean value) {
		setCustomization1(d, nameReplace, value);
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
