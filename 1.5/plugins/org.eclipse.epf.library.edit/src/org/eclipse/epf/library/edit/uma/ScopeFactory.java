package org.eclipse.epf.library.edit.uma;

import org.eclipse.epf.library.edit.uma.impl.LibraryScopeImpl;
import org.eclipse.epf.library.edit.uma.impl.ScopeImpl;

public class ScopeFactory {

	private static ScopeFactory instance = new ScopeFactory();
	public static ScopeFactory getInstance() {
		return instance;
	}
		
	private ScopeFactory() {		
	}
	
	public Scope newScope() {
		return new ScopeImpl();
	}
	
	public Scope newLibraryScope() {
		return new LibraryScopeImpl();
	}
	
}
