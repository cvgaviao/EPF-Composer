/**
 * 
 */
package org.eclipse.epf.library.edit.breakdownelement;

import java.util.Collection;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.epf.library.edit.util.TngUtil;

/**
 * @author skannoor
 *
 */
public class ActivityItemProvider extends
		org.eclipse.epf.uma.provider.ActivityItemProvider {

	public ActivityItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public boolean hasChildren(Object object) {
		return false;
	}
	
	@Override
	public String getText(Object object) {
		return TngUtil.getLabel(object, getString("_UI_Activity_type")); //$NON-NLS-1$
	}
	
	@Override
	public Object getImage(Object object) {
		Object img = TngUtil.getImage(object);
		return img != null ? img : super.getImage(object);
	}
	
	@Override
	protected void collectNewChildDescriptors(Collection newChildDescriptors, Object object) {
		return;
	}
}
