package org.eclipse.epf.library.edit.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.uma.Scope;
import org.eclipse.epf.library.edit.uma.ScopeFactory;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.util.UmaUtil;

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
		MethodConfiguration config = proc.getDefaultContext();
		if (config instanceof Scope) {
			return (Scope) config;
		} else if (config != null) {
			return null;
		}
		
		Scope scope = ScopeFactory.getInstance().newScope();

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
	
	private void addReferenceToScope(Scope scope, MethodElement element, 
			Set<MethodElement> handledReferenceSet) {
		if (handledReferenceSet.contains(element)) {
			return;
		}
		scope.addToScope(element);
		handledReferenceSet.add(element);
		
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
					for (Object obj : list) {
						addReferenceToScope(scope, (MethodElement) obj, handledReferenceSet);
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
	
}
