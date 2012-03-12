package org.eclipse.epf.library.edit.uma;

import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;

public class MethodPluginExt extends MethodElementExt {
	
	private boolean wpStatesLoaded = false;
	public MethodPluginExt(MethodPlugin plugin) {
		super(plugin);
	}	
	
	public boolean isWpStatesLoaded() {
		return wpStatesLoaded;
	}
	public void setWpStatesLoaded(boolean wpStatesLoaded) {
		this.wpStatesLoaded = wpStatesLoaded;
	}
}