package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.uma.DescriptorExt;
import org.eclipse.epf.library.edit.uma.MethodElementExt;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.Concept;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.DescriptorDescription;
import org.eclipse.epf.uma.EstimationConsiderations;
import org.eclipse.epf.uma.Example;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.Guideline;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Report;
import org.eclipse.epf.uma.ReusableAsset;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.Template;
import org.eclipse.epf.uma.ToolMentor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class DescriptorPropUtil extends MethodElementPropUtil {
	
	
	private static boolean localDebug = false;
	
	public static final String infoSeperator = "/"; 							//$NON-NLS-1$
	private static final String plus = "+"; 							//$NON-NLS-1$
	private static final String minus = "-"; 							//$NON-NLS-1$

	//Method property name strings
	public static final String DESCRIPTOR_NoAutoSyn = "descriptor_noAutoSyn"; 							//$NON-NLS-1$	
	public static final String DESCRIPTOR_CreatedByReference = "descriptor_createdByReferebce"; 		//$NON-NLS-1$
	public static final String DESCRIPTOR_Customization = "descriptor_customization"; 					//$NON-NLS-1$
	public static final String DESCRIPTOR_LocalUsingInfo = "descriptor_localUsingInfo";					//$NON-NLS-1$
	public static final String DESCRIPTOR_GreenParent = "descriptor_greenParent";						//$NON-NLS-1$
	public static final String DESCRIPTOR_ExcludeRefDelta = "descriptor_excludeRefDelta";	
	
	private static int nameReplace = 				1;		//0000000000000001
	private static int presentatioNameReplace = 	2;		//0000000000000010
	private static int briefDesReplace = 			4;		//0000000000000100
	private static int mainDesReplace = 			8;		//0000000000001000
	private static int keyConsiderReplace =		   16;		//0000000000010000
	
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
	
	//Test if the descriptor is not to be automatically synchronized
	//Specifically, if the linked element of a descriptor is selected from UI by user,
	//then we don't want to automatically synchronize it.
	public boolean isNoAutoSyn(Descriptor d) {
		if (getLinkedElement(d) == null) {
			return false;
		}
		Boolean value = getBooleanValue(d, DESCRIPTOR_NoAutoSyn);
		return value == null ? false : value.booleanValue();
	}
	
	public void setNoAutoSyn(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_NoAutoSyn, value);
	}
	
	//Test if the descriptor is created indirectly due to a reference relationship 
	public boolean isCreatedByReference(Descriptor d) {
		Boolean value = getBooleanValue(d, DESCRIPTOR_CreatedByReference);
		return value == null ? false : value.booleanValue();
	}
	
	public void setCreatedByReference(Descriptor d, boolean value) {
		setBooleanValue(d, DESCRIPTOR_CreatedByReference, value);
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
	
	public boolean isMainDesRepalce(Descriptor d) {
		if (d.getPresentation() instanceof DescriptorDescription) {
			String value = ((DescriptorDescription) d.getPresentation()).getRefinedDescription();
			if (hasNoValue(value)) {
				return false;
			}
		}
		return getCustomization(d, mainDesReplace);
	}
	
	public void setMainDesRepalce(Descriptor d, boolean value) {
		setCustomization(d, mainDesReplace, value);
	}
	
	public boolean isKeyConsiderRepalce(Descriptor d) {
		if (d.getPresentation() instanceof DescriptorDescription) {
			String value = ((DescriptorDescription) d.getPresentation()).getKeyConsiderations();
			if (hasNoValue(value)) {
				return false;
			}
		}
		return getCustomization(d, keyConsiderReplace);
	}
	
	public void setKeyConsiderRepalce(Descriptor d, boolean value) {
		setCustomization(d, keyConsiderReplace, value);
	}
	
	//Test if usedD is locally used by usingD through relationship specified by the
	//given feature.
	public boolean localUse(Descriptor usedD, Descriptor usingD, EReference feature) {
		try {
			boolean be = localUse_(usedD, usingD, feature);
			if (localDebug) {
				System.out.println("LD> localUse: " + be + 		
						", usingD: " + usingD.getName() +
						", usedD: " + usedD.getName() +
						", feature: " + feature.getName());;
						//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			}
			
			return be;
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
			if (localDebug) {
				System.out.println("LD> addLocalUse, usingD: " + usingD.getName() +
						", usedD: " + usedD.getName() + ", feature: " + feature.getName());
				//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
			addRefInfo(usedD, usingD, DESCRIPTOR_LocalUsingInfo, feature.getName());
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
	}
	
	public void removeLocalUse(Descriptor usedD, Descriptor usingD, EReference feature) {
		try {
			removeRefInfo(usedD, usingD, DESCRIPTOR_LocalUsingInfo, feature.getName());
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
	}

	public void addLocalUsingInfo(List<Descriptor> deslIst, Descriptor desc, EReference feature) {
		if (! ProcessUtil.isSynFree() || deslIst == null || desc == null || feature == null) {
			return;
		}
		
		for (Descriptor des : deslIst) {
			addLocalUse(des, desc, feature);
		}
	}
	
	public void removeLocalUsingInfo(List<Descriptor> deslIst, Descriptor desc, EReference feature) {
		if (! ProcessUtil.isSynFree() || deslIst == null || desc == null || feature == null) {
			return;
		}		

		for (Descriptor des : deslIst) {
			removeLocalUse(des, desc, feature);
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
			cus ^= maskBit;
		}
		setIntValue(d, DESCRIPTOR_Customization, cus);
	}	
	
	public MethodElement getLinkedElement(Descriptor des) {
		return ProcessUtil.getAssociatedElement(des);
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
		if (feature == up.getDescriptorDescription_RefinedDescription()) {
			return isMainDesRepalce(d);
		}
		if (feature == up.getContentDescription_KeyConsiderations()) {
			return isKeyConsiderRepalce(d);
		}
		return false;
	}
	
	//Return green parent guid
	public String getGreenParent(Descriptor d) {
		return getStringValue(d, this.DESCRIPTOR_GreenParent);
	}
	
	public void setGreenParent(Descriptor d, String value) {
		setStringValue(d, this.DESCRIPTOR_GreenParent, value);
	}
	
 	public EReference getExcludeFeature(EReference ref) {		
		return LibraryEditUtil.getInstance().getExcludeFeature(ref);
	}
	
	/**
	 * 
	 * Check all the elements in list, to see if contains elements with
	 * different type
	 * @return true single type
	 *         flase multiple type
	 * 
	 */
	public boolean checkSelection(List list, Descriptor desc, EReference ref, MethodConfiguration config) {		
		int dynamic = 0;
		int dynamicExclude = 0;
		int local = 0;
		
		for (int i = 0; i < list.size(); i++) {
			MethodElement des = (MethodElement) list.get(i);
			if (isDynamicAndExclude(des, desc, ref, config)) {
				dynamicExclude ++;
			} else if (isDynamic(des, desc, ref)) {
				dynamic ++;
			} else {
				local ++;
			}
		}
		
		if (((dynamic > 0) && (dynamicExclude > 0))
			|| ((dynamic > 0) && (local > 0))
			|| ((local > 0) && (dynamicExclude > 0))) {
			return false;
		}
		
		return true;		
	}
	
	public boolean CheckSelectionForGuidance(List list, Descriptor desc, MethodConfiguration config) {
		int dynamic = 0;
		int dynamicExclude = 0;
		int local = 0;
		
		for (int i = 0; i < list.size(); i++) {
			MethodElement des = (MethodElement) list.get(i);
			EReference ref = getGuidanceEReference((Guidance)des);
			if (isDynamicAndExclude(des, desc, ref, config)) {
				dynamicExclude ++;
			} else if (isGuidanceDynamic(des, desc, config)) {
				dynamic ++;
			} else {
				local ++;
			}
		}
		
		if (((dynamic > 0) && (dynamicExclude > 0))
			|| ((dynamic > 0) && (local > 0))
			|| ((local > 0) && (dynamicExclude > 0))) {
			return false;
		}
		
		return true;
	}
	
	public Descriptor getGreenParentDescriptor(Descriptor des) {
		String guid = getGreenParent(des);
		if (guid == null) {
			return null;
		}
		return (Descriptor) LibraryEditUtil.getInstance().getMethodElement(guid);
	}	
	
	//positive = true:  feature name appended with "+" -> extra exclude added
	//positive = false: feature name appended with "-" -> toggle off parent's exclude
	public void addExcludeRefDelta(Descriptor des, MethodElement referenced, EReference feature, boolean positive) {
		String refName = feature.getName() + (positive ? plus : minus);
		try {
			addRefInfo(des, referenced, DESCRIPTOR_ExcludeRefDelta, refName);
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
	}
	
	public List<MethodElement> getExcludeRefDeltaList(Descriptor des,			
			EReference feature, boolean positive) {
		
		String refName = feature.getName() + (positive ? plus : minus);
		try {
			return getExcludeRefDeltaList_(des, refName);			
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
		
		return null;
	}
	
	private List<MethodElement> getExcludeRefDeltaList_(Descriptor des, String refName) { 		
		String value = getStringValue(des, DESCRIPTOR_ExcludeRefDelta);
		
		if (value == null || value.length() == 0) {
			return null;
		}
		
		String[] infos = value.split(infoSeperator);
		if (infos == null || infos.length == 0) {
			return null;
		}
		
		List<MethodElement> deltaList = new ArrayList<MethodElement>();
		int sz = infos.length / 2; 		
		for (int i = 0; i < sz; i++) {
			int i1 = i*2;
			int i2 = i1 + 1;
			String iGuid = infos[i1];
			String iFeature = infos[i2];
			if (iFeature.endsWith(refName)) {
				MethodElement element = LibraryEditUtil.getInstance().getMethodElement(iGuid);
				if (element != null) {
					deltaList.add(element);
				}
			} 
		}		
		
		return deltaList;		
	}
	
	public void removeExcludeRefDelta(Descriptor des, MethodElement referenced, EReference feature, boolean positive) {
		String refName = feature.getName() + (positive ? plus : minus);
		try {
			removeRefInfo(des, referenced, DESCRIPTOR_ExcludeRefDelta, refName);
		} catch (Throwable e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
	}
	
	private void addRefInfo(Descriptor descriptor, MethodElement referenced, String propName, String refName) {
		String oldValue = getStringValue(descriptor, propName);
		String newValue = referenced.getGuid().concat(infoSeperator).concat(refName);

		if (oldValue != null && oldValue.length() > 0) {			
			String[] infos = oldValue.split(infoSeperator); 
			
			int sz = infos.length / 2; 		
			for (int i = 0; i < sz; i++) {
				int i1 = i*2;
				int i2 = i1 + 1;
				String iGuid = infos[i1];
				String iFeature = infos[i2];
				if (iGuid.equals(referenced.getGuid()) && iFeature.equals(refName)) {
					return;
				} 
			}
			
			newValue = oldValue.concat(infoSeperator).concat(newValue);
		}				
		setStringValue(descriptor, propName, newValue);
	}
		
	private void removeRefInfo(Descriptor descriptor, MethodElement referenced, String propName, String refName) {
		String oldValue = getStringValue(descriptor, propName);
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
				if (iGuid.equals(referenced.getGuid()) && iFeature.equals(refName)) {
					removed = true;		
				} else {
					if (newValue.length() > 0) {
						newValue = newValue.concat(infoSeperator);
					}
					newValue = newValue.concat(iGuid.concat(infoSeperator).concat(iFeature));
				}
			}

		}
		
		if (removed) {
			setStringValue(descriptor, propName, newValue);
		}

	}
	
	public List<? extends Descriptor> getCustomizingChildren(Descriptor des) {
		if (des == null) {
			return null;
		}
		DescriptorExt ext = (DescriptorExt) getExtendObject(des, false);

		return ext == null ? null : ext.getCustomizingChildren();
	}
	
	public void addToCustomizingChildren(Descriptor parent, Descriptor child) {
		if (parent == null || child == null) {
			return;
		}
		DescriptorExt ext = (DescriptorExt) getExtendObject(parent, true);
		ext.addToCustomizingChildren(child);
	}
	
	public boolean isDynamicAndExclude(Object obj, Descriptor desc,
			EReference ref, MethodConfiguration config) {
		return LibraryEditUtil.getInstance().isDynamicAndExclude(obj, desc,
				ref, config);
	}

	public boolean isDynamic(Object obj, Descriptor desc, EReference ref) {
		return LibraryEditUtil.getInstance().isDynamic(obj, desc, ref);
	}
	
	public boolean isGuidanceDynamic(Object obj, Descriptor desc,
			MethodConfiguration config) {
		return LibraryEditUtil.getInstance().isGuidanceDynamic(obj, desc, config);
	}
	
	public boolean isDescriptor(BreakdownElement element) {
		if ((element instanceof TaskDescriptor) || (element instanceof RoleDescriptor)
				|| (element instanceof WorkProductDescriptor)) {
			return true;
		}
		
		return false;
	}
	
	public EReference getGuidanceEReference(Guidance item) {
		EReference ref = null;
		
		if (item instanceof Checklist) {		
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Checklists();		
		} else if (item instanceof Concept) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Concepts();
		} else if (item instanceof Example) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Examples();
		} else if (item instanceof SupportingMaterial) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_SupportingMaterials();
		} else if (item instanceof Guideline) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Guidelines();
		} else if (item instanceof ReusableAsset) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_ReusableAssets();
		}else if (item instanceof Template) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Templates();
		}else if (item instanceof Report) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Reports();
		}else if (item instanceof ToolMentor) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Toolmentor();
		}else if (item instanceof EstimationConsiderations) {
			ref = UmaPackage.eINSTANCE.getBreakdownElement_Estimationconsiderations();
		}
		
		return ref;
	}
	
	protected MethodElementExt createExtendObject(MethodElement element) {
		if (element instanceof Descriptor) {
			return new DescriptorExt((Descriptor) element);
		}
		return super.createExtendObject(element);
	}
	
}