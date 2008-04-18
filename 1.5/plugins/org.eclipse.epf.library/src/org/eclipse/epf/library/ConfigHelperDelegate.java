package org.eclipse.epf.library;

import java.util.List;

import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;

public class ConfigHelperDelegate {

	
	/**
	 * Test if pkg is a system package of plugin
	 * @param plugin
	 * @param pkg
	 * @return
	 */
	public boolean isSystemPackage(MethodPlugin plugin, MethodPackage pkg) {
		return TngUtil.getAllSystemPackages(plugin).contains(pkg);
	}
	
}
