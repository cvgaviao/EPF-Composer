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
package org.eclipse.epf.authoring.ui.forms;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Interface to be used add columns on description tab of all editors
 * 
 * @author Shilpa Toraskar
 * @since 1.5
 */
public interface IColumnProvider   {

	/**
	 * Set composite for column
	 * @param parent composite
	 */
	public Composite setColumn(FormToolkit toolkit, Composite parent);
	
}
