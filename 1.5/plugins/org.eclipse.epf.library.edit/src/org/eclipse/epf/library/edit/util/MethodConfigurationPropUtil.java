package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;


public class MethodConfigurationPropUtil extends MethodElementPropUtil {

	public static final String Config_elementsUnslectedPkgs = "config_elementsUnslectedPkgs"; 	//$NON-NLS-1$
	public static final String Config_selectedElements = "config_selectedElements"; 			//$NON-NLS-1$
	public static final String Config_deselectedElements = "config_deselectedElements"; 		//$NON-NLS-1$	
	
	private static MethodConfigurationPropUtil methodConfigurationPropUtil = new MethodConfigurationPropUtil();

	public static MethodConfigurationPropUtil getMethodConfigurationPropUtil() {
		return methodConfigurationPropUtil;
	}

	public static MethodConfigurationPropUtil getMethodConfigurationPropUtil(
			IActionManager actionManager) {
		return new MethodConfigurationPropUtil(actionManager);
	}
	
	protected MethodConfigurationPropUtil() {		
	}
	
	protected MethodConfigurationPropUtil(IActionManager actionManager) {
		super(actionManager);
	}	
	
	public void setElementsUnslectedPkgsProp(MethodConfiguration config, Set<ContentPackage> pkgs) {
		String value = "";	//$NON-NLS-1$
		List<String> guidList = new ArrayList<String>();
		if (pkgs != null) {
			for (ContentPackage pkg : pkgs) {
				guidList.add(pkg.getGuid());
			}
			Collections.sort(guidList);
			for (String guid : guidList) {
				if (value.length() != 0) {
					value += infoSeperator;
				}
				value += guid;
			}
		}
		setStringValue(config, Config_elementsUnslectedPkgs, value);		
	}
	
	public Set<ContentPackage> getElementsUnslectedPkgs(
			MethodConfiguration config) {
		Set<ContentPackage> pkgs = new HashSet<ContentPackage>();

		String value = getStringValue(config, Config_elementsUnslectedPkgs);
		String[] guids = value == null ? null : value.split(infoSeperator);

		if (guids == null || guids.length == 0) {
			return pkgs;
		}

		for (int i = 0; i < guids.length; i++) {
			MethodElement element = LibraryEditUtil.getInstance()
					.getMethodElement(guids[i]);
			if (element instanceof ContentPackage) {
				pkgs.add((ContentPackage) element);
			}
		}

		return pkgs;
	}
	
}
