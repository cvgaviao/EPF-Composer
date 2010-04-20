package org.eclipse.epf.library.edit.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.uma.Scope;
import org.eclipse.epf.library.edit.uma.ScopeFactory;
import org.eclipse.epf.uma.ContentElement;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessElement;

public class ProcessScopeUtil {

	private static ProcessScopeUtil instance = new ProcessScopeUtil();
	
	private Scope libraryScope = ScopeFactory.getInstance().newLibraryScope();
	private Set<Scope> scopeInEditdSet = new HashSet<Scope>();

	public static final int ScopeType_Config = 0;
	public static final int ScopeType_Process = 1;
	public static final int ScopeType_Library = 2;
	
	private int scopeType = ScopeType_Config;
	
	public static ProcessScopeUtil getInstance() {
		return instance;
	}	
	
	public ProcessScopeUtil() {		
	}	
	
	public void setElemementSelectionScopeType(int type) {	
		scopeType = type;
	}
	
	public int getElemementSelectionScopeType() {
		return scopeType;
	}
	
	public Scope loadScope(Process proc) {
		if (proc == null) {
			return null;
		}
		if (proc.getDefaultContext() != null && !(proc.getDefaultContext() instanceof Scope)) {
			return null;
		}
		
		Scope scope = getScope(proc); 
		if (scope != null) {
			updateScope(proc);
			return scope;
		}
			
		scope =	ScopeFactory.getInstance().newProcessScope();

		addReferenceToScope(scope, proc, new HashSet<MethodElement>());
		proc.setDefaultContext(scope);
		proc.getValidContext().add(scope);
		
		return scope;
	}
	
	public void updateScope(Process proc) {
		Scope scope = getScope(proc);
		if (scope == null) {
			return;
		}
		scope.clearAll();
		addReferenceToScope(scope, proc, new HashSet<MethodElement>());

	}
	
	public void addReferenceToScope(Scope scope, MethodElement element, 
			Set<MethodElement> handledReferenceSet) {
		if (handledReferenceSet.contains(element)) {
			return;
		}
		handledReferenceSet.add(element);		
		scope.addToScope(element);
		
		if (element instanceof ContentElement) {
			
		} else if (element instanceof ProcessElement) {
			
		} else {
			return;
		}

		
		for (EStructuralFeature f : element.eClass()
				.getEAllStructuralFeatures()) {
			if (!(f instanceof EReference)) {
				continue;
			}
			EReference feature = (EReference) f;
			if (feature.isContainer() || feature.isContainment()) {
				continue;
			}

			Object value = element.eGet(feature);
			if (value == null) {
				continue;
			}
			
			if (value instanceof MethodElement) {
				addReferenceToScope(scope, (MethodElement) value, handledReferenceSet);
			} else if (value instanceof List) {
				List list = (List) value;
				if (list.size() > 0 && list.get(0) instanceof MethodElement) {
					for (int i = 0; i < list.size(); i++) {
						addReferenceToScope(scope, (MethodElement) list.get(i), handledReferenceSet);
					}
				}
			}
		}
		
	}
		
	public Scope getScope(Process proc) {
		if (proc == null || !(proc.getDefaultContext() instanceof Scope)) {
			return null;
		}
		
		Scope scope = (Scope) proc.getDefaultContext();		
		return scope;
	}
	
	public Scope getLibraryScope() {
		return libraryScope;
	}
	
	public void beginProcessEdit(Scope scope) {
		scopeInEditdSet.add(scope);
	}
	
	public void endProcesEdit(Scope scope) {
		scopeInEditdSet.remove(scope);
	}

	public Set<Scope> getScopeInEditdSet() {
		return scopeInEditdSet;
	}
	
	public boolean isConfigFree(Process process) {
		boolean result = getScope(process) != null;
		
		return result;
	}
	
}
