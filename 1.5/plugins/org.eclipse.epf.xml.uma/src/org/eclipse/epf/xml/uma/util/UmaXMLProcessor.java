/**
 * <copyright>
 * </copyright>
 *
 * $Id: UmaXMLProcessor.java,v 1.1 2008/01/15 08:52:51 jtham Exp $
 */
package org.eclipse.epf.xml.uma.util;

import java.util.Map;

import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.xmi.util.XMLProcessor;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * This class contains helper methods to serialize and deserialize XML documents
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class UmaXMLProcessor extends XMLProcessor {
	/**
	 * Public constructor to instantiate the helper.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UmaXMLProcessor() {
		super((EPackage.Registry.INSTANCE));
		UmaPackage.eINSTANCE.eClass();
	}
	
	/**
	 * Register for "*" and "xml" file extensions the UmaResourceFactoryImpl factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected Map getRegistrations() {
		if (registrations == null) {
			super.getRegistrations();
			registrations.put(XML_EXTENSION, new UmaResourceFactoryImpl());
			registrations.put(STAR_EXTENSION, new UmaResourceFactoryImpl());
		}
		return registrations;
	}

} //UmaXMLProcessor
