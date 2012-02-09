package org.eclipse.epf.library.edit.uma;

import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;

public class MethodPluginExt extends MethodElementExt {
	
	private boolean wpStatesLoaded = false;
	public MethodPluginExt(MethodPlugin plugin, ExtendObject oldObj) {
		super(plugin, oldObj);
	}	
	
	@Override
	protected void copy(ExtendObject oldObj) {
		super.copy(oldObj);
		if (! (oldObj instanceof MethodPluginExt)) {
			return;
		}
		MethodPluginExt old = (MethodPluginExt) oldObj;			
		wpStatesLoaded = old.wpStatesLoaded;
	}
	
	public boolean isWpStatesLoaded() {
		return wpStatesLoaded;
	}
	public void setWpStatesLoaded(boolean wpStatesLoaded) {
		this.wpStatesLoaded = wpStatesLoaded;
	}
}