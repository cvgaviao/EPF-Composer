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
package org.eclipse.epf.library.layout.elements;

import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.common.utils.FileUtil;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.layout.ElementLayoutManager;
import org.eclipse.epf.library.layout.util.XmlElement;
import org.eclipse.epf.library.util.ResourceHelper;
import org.eclipse.epf.uma.ContentDescription;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.AssociationHelper;


/**
 * The element layout for a Practice
 * 
 * @author Weiping Lu
 * @since 1.5
 */
public class PracticeLayout extends AbstractElementLayout {

	public PracticeLayout() {
		super();
	}

	public void init(ElementLayoutManager layoutManager, MethodElement element) {
		super.__init(layoutManager, element);
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getXmlElement(boolean)
	 */
	public XmlElement getXmlElement(boolean includeReferences) {
		XmlElement elementXml = super.getXmlElement(includeReferences);

		if (false & includeReferences) {
			//List 

			//
			List contentElements = ConfigurationHelper.calc0nFeatureValue(
					super.element, AssociationHelper.Example_ContentElements,
					layoutManager.getElementRealizer());

			List activities = ConfigurationHelper.calc0nFeatureValue(
					super.element, AssociationHelper.Example_BreakdownElements,
					layoutManager.getElementRealizer());

			contentElements.addAll(activities);

			addReferences(AssociationHelper.Example_ContentElements,
					elementXml, "contentElements", contentElements); //$NON-NLS-1$

		}

		return elementXml;
	}
	
	private List getAllReferences() {
		//UmaPackage.Literals.PRACTICE__CONTENT_REFERENCES
		//UmaPackage.Literals.PRACTICE__ACTIVITY_REFERENCES
		//UmaPackage.Literals.PRACTICE__SUB_PRACTICES

		
		return null;
	}

}
