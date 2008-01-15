/**
 * <copyright>
 * </copyright>
 *
 * $Id: UmaResourceImpl.java,v 1.1 2008/01/15 08:52:51 jtham Exp $
 */
package org.eclipse.epf.xml.uma.util;

import java.io.IOException;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.xmi.XMLSave;
import org.eclipse.emf.ecore.xmi.impl.XMLResourceImpl;

/**
 * <!-- begin-user-doc -->
 * The <b>Resource </b> associated with the package.
 * <!-- end-user-doc -->
 * @see org.eclipse.epf.xml.uma.util.UmaResourceFactoryImpl
 * @generated
 */
public class UmaResourceImpl extends XMLResourceImpl {
	/**
	 * Creates an instance of the resource.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param uri the URI of the new resource.
	 * @generated
	 */
	public UmaResourceImpl(URI uri) {
		super(uri);
	}

	////////////////////////////////////////////////////////////////////////
	// customization here, Jinhua Xi 04/28/2006
	////////////////////////////////////////////////////////////////////////
	
	/*
	 * Javadoc copied from interface.
	 */
	public void save(Map options) throws IOException {
		super.save(options);
	}

	protected XMLSave createXMLSave() {
		// return super.createXMLSave();
		return new UmaXMLSaveImpl(createXMLHelper());
	}
	
} //UmaResourceImpl
