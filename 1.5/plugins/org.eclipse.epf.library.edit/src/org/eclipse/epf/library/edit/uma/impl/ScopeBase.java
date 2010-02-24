package org.eclipse.epf.library.edit.uma.impl;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.library.edit.uma.Scope;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.Kind;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.ecore.IModelObject;
import org.eclipse.epf.uma.ecore.Property;
import org.eclipse.epf.uma.ecore.Type;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.impl.MethodConfigurationImpl;

/**
 * 
 * @author Weiping Lu
 * @since 1.5.1
 *
 */
public abstract class ScopeBase extends MethodConfigurationImpl implements Scope {

	private static final String defaultName = "-- None --";		//$NON-NLS-1$ 
	
	public ScopeBase() {
		super(null);
		eSetDeliver(false);
		setName(defaultName);
	}
	
	public abstract void addToScope(MethodElement element);

	public abstract boolean inScope(MethodElement element);
	
	public void clearAll() {		
		Scope scope = null;
	}

}
