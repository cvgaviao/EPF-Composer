package org.eclipse.epf.library.edit.navigator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.epf.library.edit.element.ContentPackageItemProvider;

public class ConfigContentPackageItemProvider extends
		ContentPackageItemProvider {
	
	public ConfigContentPackageItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}
	
	@Override
	public Collection getChildren(Object object) {
		List result = new ArrayList();
		result.add(new LeafElementsItemProvider(getAdapterFactory()));		
		Collection children = super.getChildren(object);		
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
