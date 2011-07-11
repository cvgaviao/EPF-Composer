package org.eclipse.epf.library.edit.navigator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.epf.library.edit.element.ContentPackageItemProvider;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.util.UmaUtil;

public class ConfigContentPackageItemProvider extends
		ContentPackageItemProvider {
	
	public static boolean oldCode = true;
	
	public ConfigContentPackageItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}
	
	@Override
	public Collection getChildren(Object object) {
		if (oldCode) {
			return super.getChildren(object);
		}
		List result = new ArrayList();
		if (object instanceof ContentPackage) {
			ContentPackage pkg = (ContentPackage) object;
			MethodPlugin plugin = UmaUtil.getMethodPlugin(pkg);
			if (plugin != null && !TngUtil.getAllSystemPackages(plugin).contains(pkg)) {
				result.add(new LeafElementsItemProvider(getAdapterFactory()));
			}
		}		
		
		Collection children = super.getChildren(object);
		if (result.isEmpty()) {
			return children;
		}
		result.addAll(children);
		return result;
	}
	
	public static class LeafElementsItemProvider extends ConfigContentPackageItemProvider {		
		public LeafElementsItemProvider(AdapterFactory adapterFactory) {
			super(adapterFactory);
		}
		
		@Override
		public Collection getChildren(Object object) {		
			return Collections.EMPTY_LIST;		
		}
		
	}
	
}
