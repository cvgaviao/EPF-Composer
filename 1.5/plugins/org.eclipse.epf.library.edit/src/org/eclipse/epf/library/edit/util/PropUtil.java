package org.eclipse.epf.library.edit.util;

import java.util.Map;

import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.uma.MethodElementExt;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.util.ModifiedTypeMeta;
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
	
	public ModifiedTypeMeta getMdtMeta(MethodElement element) {
		try {
			return getMdtData(element);			
		} catch (Exception e) {
			LibraryEditPlugin.getDefault().getLogger().logError(e);
		}
		return null;
	}
	
	private  ModifiedTypeMeta getMdtData(MethodElement element)  throws Exception {
		MethodElementExt extendObject = getExtendObject(element, true);
		if (extendObject != null && extendObject.getModifiedTypeMeta() instanceof ModifiedTypeMeta) {
			ModifiedTypeMeta meta = (ModifiedTypeMeta) extendObject.getModifiedTypeMeta();
			return meta == ModifiedTypeMeta.noneValue ? null : meta;
		}
		ModifiedTypeMeta meta = new ModifiedTypeMeta();
		
		PropXmlEditUtil xmlEditUtil = new PropXmlEditUtil(element, this);
		xmlEditUtil.retrieveMdtData(meta);
		if (extendObject == null) {
			return meta.getId() == null ? null : meta;
		}
		if (meta.getId() == null) {
			meta = ModifiedTypeMeta.noneValue;
		} else {
			meta = LibraryEditUtil.getInstance().getModifiedType(meta.getId());
		}
		extendObject.setUserDefinedTypeMeta(meta);
		return meta == ModifiedTypeMeta.noneValue ? null : meta;
	}
	
	private static class PropXmlEditUtil extends XmlEditUtil {
		private MethodElement element;
		public static final String _id = "id"; 					//$NON-NLS-1$	
		
		public PropXmlEditUtil(MethodElement element, MethodElementPropUtil propUtil) {
			super(propUtil);
			this.element = element;
		}	
		
		public void retrieveMdtData(ModifiedTypeMeta meta) throws Exception {
			Map<String, String> map = null;
			
			String xmlString = getPropUtil().getStringValue(element, Me_mdtData);
			if (xmlString == null || xmlString.trim().length() == 0) {
				return;
			}
			Element firstElement = loadDocumentAndGetFirstElement(xmlString);
			if (firstElement == null) {
				return;
			}				
			String value = firstElement.getAttribute(_id);
			if (value != null && value.length() > 0) {
				meta.setId(value);
			}
			for (String name : UserDefinedTypeMeta.rteNames) {
				value = firstElement.getAttribute(name);
				if (value != null && value.length() > 0) {
					map.put(name, value);
				}
			}
		}
	}
	
}
