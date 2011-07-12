package org.eclipse.epf.library.edit.navigator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	
	private Map<ContentPackage, LeafElementsItemProvider> map = new HashMap<ContentPackage, LeafElementsItemProvider>();
	
	@Override
	public Collection getChildren(Object object) {
		if (oldCode) {
			return super.getChildren(object);
		}
		List modifiedChildren = new ArrayList();
		if (object instanceof ContentPackage) {
			ContentPackage pkg = (ContentPackage) object;
			MethodPlugin plugin = UmaUtil.getMethodPlugin(pkg);
			if (plugin != null && !TngUtil.getAllSystemPackages(plugin).contains(pkg)) {
				LeafElementsItemProvider p = map.get(pkg);
				if (p == null) {
					p = new LeafElementsItemProvider(getAdapterFactory());
					map.put(pkg, p);
				}
				modifiedChildren.add(p);
			}
		}		
		
		Collection children = super.getChildren(object);
		if (modifiedChildren.isEmpty()) {
			return children;
		}
		modifiedChildren.addAll(children);
		return modifiedChildren;
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
	
	public void dispose() {
		if (map != null) {
			map.clear();
			map = null;
		}
		super.dispose();
	}

	
}
