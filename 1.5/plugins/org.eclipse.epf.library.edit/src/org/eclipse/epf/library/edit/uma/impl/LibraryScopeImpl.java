package org.eclipse.epf.library.edit.uma.impl;

import org.eclipse.epf.uma.MethodElement;

public class LibraryScopeImpl extends ScopeBase {

	public LibraryScopeImpl() {		
	}
	
	@Override
	public boolean inScope(MethodElement element) {
		return true;
	}
	
	@Override
	public void addToScope(MethodElement element) {
		
	}
}
