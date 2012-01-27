package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.meta.internal.ModifiedTypeMetaImpl;
import org.eclipse.epf.library.edit.uma.ExtendReferenceMap;
import org.eclipse.epf.library.edit.uma.MethodElementExt;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.util.ExtendedReference;
import org.eclipse.epf.uma.util.MetaElement;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.epf.uma.util.UserDefinedTypeMeta;
import org.w3c.dom.Element;

public class PropUtil extends MethodElementPropUtil {

	public static final String Me_mdtData = "me_mdtData";					//$NON-NLS-1$
	
	private static PropUtil propUtil = new PropUtil();
	public static PropUtil getPropUtil(IActionManager actionManager) {
		return new PropUtil(actionManager);
	}
	
	public static PropUtil getPropUtil() {
		return propUtil;
	}
		
	protected PropUtil() {		
	}
	
	protected PropUtil(IActionManager actionManager) {
		super(actionManager);
	}
	
	public List<MethodElement> getExtendedReferenceList(MethodElement element, 	ExtendedReference meta, boolean toModify) {
		List<MethodElement> value = (List<MethodElement>) getReferenceValue(meta.getGlobalId(), element, toModify);
		if (value == null) {
			return new ArrayList<MethodElement>();
		}
		return value;
	}
	
	public ModifiedTypeMeta getMdtMeta(MethodElement element) {
		try {
			return getMdtData(element);			
		} catch (Exception e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
		return null;
	}
	
	private static String umaTypeScope = "org.eclipse.epf.uma.";		//$NON-NLS-1$	
	public ModifiedTypeMeta getGlobalMdtMeta(MethodElement element) {
		String id = umaTypeScope + element.eClass().getName();
		ModifiedTypeMeta meta = LibraryEditUtil.getInstance().getModifiedType(id);
		return meta;
	}
	
	private  ModifiedTypeMeta getMdtData(MethodElement element)  throws Exception {
		MethodElementExt extendObject = getExtendObject(element, true);
		if (extendObject != null && extendObject.getModifiedTypeMeta() instanceof ModifiedTypeMeta) {
			ModifiedTypeMeta meta = (ModifiedTypeMeta) extendObject.getModifiedTypeMeta();
			return meta == ModifiedTypeMeta.noneValue ? null : meta;
		}
		ModifiedTypeMeta meta = new ModifiedTypeMetaImpl();
		
		PropXmlEditUtil xmlEditUtil = new PropXmlEditUtil(element, this);
		xmlEditUtil.retrieveMdtData(meta);
		if (extendObject == null) {
			return meta.getId() == null ? null : meta;
		}
		if (meta.getId() == null) {
			meta = null;
		} else {
			meta = LibraryEditUtil.getInstance().getModifiedType(meta.getId());
		}
		extendObject.setModifiedTypeMeta(meta == null ? MetaElement.noneValue : meta);
		return meta;
	}
	
	private static class PropXmlEditUtil extends XmlEditUtil {
		private MethodElement element;
		public static final String _id = "id"; 					//$NON-NLS-1$	
		
		public PropXmlEditUtil(MethodElement element, MethodElementPropUtil propUtil) {
			super(propUtil);
			this.element = element;
		}	
		
		public void retrieveMdtData(ModifiedTypeMeta meta) throws Exception {
			if (meta instanceof ModifiedTypeMetaImpl) {
				return;
			}
			Map<String, String> map = null;
			
			String xmlString = getPropUtil().getStringValue(element, Me_mdtData);
			if (xmlString == null || xmlString.trim().length() == 0) {
				return;
			}
			Element firstElement = loadDocumentAndGetFirstElement(xmlString);
			if (firstElement == null) {
				return;
			}				
			((ModifiedTypeMetaImpl) meta).parseElement(firstElement);
		}
	}
	
	public void addOpposite(ExtendedReference reference, MethodElement thisElement, MethodElement otherElement) {
		ExtendReferenceMap map = getCachedExtendReferenceMap(thisElement, false);
		if (map == null) {
			return;
		}
		map.addOpposite(reference, otherElement);
	}
	
	public void removeOpposite(ExtendedReference reference, MethodElement thisElement, MethodElement otherElement) {
		ExtendReferenceMap map = getCachedExtendReferenceMap(thisElement, false);
		if (map == null) {
			return;
		}
		map.removeOpposite(reference, otherElement);
	}
	
	public MethodElement getElement(MethodElement ownerElement, String propName) {
		if (ownerElement == null) {
			return null;
		}
		String guid = this.getStringValue(ownerElement, propName);
		MethodElement element = LibraryEditUtil.getInstance().getMethodElement(guid);
		if (! UmaUtil.isInLibrary(element)) {
			return null;
		}
		return element;
	}
	
	public void setElement(MethodElement ownerElement, String propName, MethodElement element) {
		if (element != null) {
			setStringValue(ownerElement, propName, element.getGuid());
		}
	}

}
