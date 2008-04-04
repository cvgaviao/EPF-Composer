/*******************************************************************************
 * Licensed Materials - Property of IBM
 * (c) Copyright IBM Corporation 2007-2008. All Rights Reserved.
 * 
 * Note to U.S. Government Users Restricted Rights:
 * Use, duplication or disclosure restricted by GSA ADP Schedule
 * Contract with IBM Corp. 
 *******************************************************************************/
package org.eclipse.epf.search.ui.internal;

import org.eclipse.swt.widgets.Composite;

/**
 * @author Phong Nguyen Le
 * @since 7.5
 *
 */
public interface IMethodSearchScopeGroupFactory {
	
	IMethodSearchScopeGroup createSearchScopeGroup(Composite composite);

}
