package org.eclipse.epf.library;

import java.util.List;
import java.util.Set;

import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.persistence.ILibraryResourceSet;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;

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
	
	public void loadOppositeFeatures(ILibraryResourceSet resouceSet,
			List<OppositeFeature> oppositeFeatures, Set<String> GUIDs) {		
		resouceSet.loadOppositeFeatures(oppositeFeatures, GUIDs);
	}
	
}
