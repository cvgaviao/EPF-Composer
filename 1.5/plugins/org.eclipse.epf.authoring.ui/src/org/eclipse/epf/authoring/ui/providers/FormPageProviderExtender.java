//------------------------------------------------------------------------------
// Copyright (c) 2005, 2012 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.authoring.ui.providers;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.epf.authoring.ui.forms.AssociationFormPage;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.UmaPackage;

/**
 * @author Weiping Lu
 */
public class FormPageProviderExtender {

	private AssociationFormPage formPage;

	public FormPageProviderExtender(AssociationFormPage formPage) {
		this.formPage = formPage;
	}
	
	public AssociationFormPage getFormPage() {
		return formPage;
	}
	
	public AssociationFormLabelProvider newLabelProvider(AdapterFactory adapterFactory, int ix) {
			return new AssociationFormLabelProvider(adapterFactory, this, ix);
	}
	
}
