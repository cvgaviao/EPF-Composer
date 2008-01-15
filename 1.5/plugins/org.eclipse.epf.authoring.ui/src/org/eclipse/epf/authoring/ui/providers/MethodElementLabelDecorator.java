//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.authoring.ui.providers;

import org.eclipse.core.resources.IResource;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.persistence.util.PersistenceUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.PlatformUI;

/**
 * Label decorator for method element
 * 
 * @author Phong Nguyen Le
 * @since  1.0
 */
public class MethodElementLabelDecorator implements ILabelDecorator {
	private static ILabelDecorator delegateDecorator;
	
	private static ILabelDecorator getDelegateDecorator() {
		if (delegateDecorator == null) {
			delegateDecorator = PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator();
		}
		return delegateDecorator;
	}
	
	private static IResource getWorkspaceResource(Object element) {
		element = TngUtil.unwrap(element);
		// don't decorate icon of predefined elements in library view b/c some of them are shown as UI folder
		//
		if(element instanceof MethodElement && TngUtil.isPredefined((MethodElement) element)) {
			return null;
		}
		return PersistenceUtil.getWorkspaceResource(element);
	}
	
	public MethodElementLabelDecorator() {
		
	}

	/**
	 * @see org.eclipse.jface.viewers.ILabelDecorator#decorateImage(org.eclipse.swt.graphics.Image, java.lang.Object)
	 */
	public Image decorateImage(Image image, Object element) {		
		IResource wsRes = getWorkspaceResource(element);
		if(wsRes != null) {
			image = getDelegateDecorator().decorateImage(image, wsRes);
		}
		return getDelegateDecorator().decorateImage(image, element);
	}

	/**
	 * @see org.eclipse.jface.viewers.ILabelDecorator#decorateText(java.lang.String, java.lang.Object)
	 */
	public String decorateText(String text, Object element) {
		return text;
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void addListener(ILabelProviderListener listener) {
		getDelegateDecorator().addListener(listener);
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
	 */
	public void dispose() {
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
	 */
	public boolean isLabelProperty(Object element, String property) {
		IResource wsRes = getWorkspaceResource(element);
		if(wsRes != null) {
			return getDelegateDecorator().isLabelProperty(wsRes, property);
		}
		return false;
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
	public void removeListener(ILabelProviderListener listener) {
		getDelegateDecorator().removeListener(listener);
	}

	public static void clearDecorationCache() {
		IDecoratorManager decoratorManager = PlatformUI.getWorkbench().getDecoratorManager();
		decoratorManager.update("org.eclipse.epf.authoring.ui.providers.MethodElementLightweightLabelDecorator"); //$NON-NLS-1$
	}
}
