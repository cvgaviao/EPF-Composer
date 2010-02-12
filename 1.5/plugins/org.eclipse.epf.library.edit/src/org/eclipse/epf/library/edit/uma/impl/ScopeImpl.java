package org.eclipse.epf.library.edit.uma.impl;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.epf.library.edit.uma.Scope;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.util.UmaUtil;

public class ScopeImpl extends ScopeBase {
	
	private Set<MethodPlugin> plugins = new HashSet<MethodPlugin>();

	public ScopeImpl() {		
	}
	
	public boolean inScope(MethodElement element) {
		MethodPlugin plugin = UmaUtil.getMethodPlugin(element);
		return plugins.contains(plugin);
	}

	public void addToScope(MethodElement element) {
		MethodPlugin plugin = UmaUtil.getMethodPlugin(element);
		if (plugin == null) {
			return;
		}
		if (! plugins.contains(plugin)) {
			plugins.add(plugin);
			getMethodPluginSelection().add(plugin);
		}
		if (Scope.debug) {
			System.out.println("LD> addToScope: " + element);//$NON-NLS-1$
			System.out.println("LD> this: " + super.toString());//$NON-NLS-1$	
			System.out.println("LD> plugins: \n" + this);//$NON-NLS-1$	

			
		}
	}
	
	public void clearAll() {
		plugins.clear();
	}
	
	public String toString() {
		String str = ""; //$NON-NLS-1$
		for (MethodPlugin p : plugins) {
			str += p + "\n"; //$NON-NLS-1$
		}
		
		return str;
	}
	
}
