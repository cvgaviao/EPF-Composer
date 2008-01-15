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
package org.eclipse.epf.library.edit.process.command;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.eclipse.emf.common.command.AbstractCommand;
import org.eclipse.emf.common.util.EList;
import org.eclipse.epf.library.edit.command.IResourceAwareCommand;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;


/**
 * Command to move up the element in breakdown structure
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public class MoveUpCommand extends AbstractCommand implements
		IResourceAwareCommand {
	private Activity activity;

	private Collection modifiedResources;

	private int elementLocation = 0;

	private int transferLocation = 0;

	private Object elementObj;

	private Collection eClasses;
	
	private boolean adjacent = false;

	/**
	 * 
	 */
	public MoveUpCommand(Activity activity, Object elementObj,
			Collection eClasses) {
		super();
		this.activity = activity;
		this.elementObj = elementObj;
		this.eClasses = eClasses;

		this.modifiedResources = new HashSet();
		if (activity.eResource() != null) {
			modifiedResources.add(activity.eResource());
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#execute()
	 */
	public void execute() {
		List allElements = activity.getBreakdownElements();

		for (int i = 0; i < allElements.size(); i++) {
			Object obj = allElements.get(i);
			if (obj.equals(elementObj)) {
				elementLocation = i;
				break;
			}
		}
		for (int i = elementLocation - 1; i >= 0; i--) {
			Object obj = allElements.get(i);
			if (TngUtil.isEClassInstanceOf(eClasses, obj)) {
				transferLocation = i;
				break;
			}
		}
		
		BreakdownElement prev =  (BreakdownElement) allElements.get(transferLocation);		
		if(prev.getPresentedAfter() == elementObj) {
			adjacent = true;
		}

		redo();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#redo()
	 */
	public void redo() {
		if(adjacent) {
			// swap "presented-after" element
			//
			BreakdownElement prev =  (BreakdownElement) activity.getBreakdownElements().get(transferLocation);		
			BreakdownElement e = (BreakdownElement) elementObj;
			prev.setPresentedAfter(e.getPresentedAfter());
			e.setPresentedAfter(prev);
		}
		((EList) activity.getBreakdownElements()).move(transferLocation,
				elementLocation);
	}

	public void undo() {
		((EList) activity.getBreakdownElements()).move(elementLocation,
				transferLocation);
		if(adjacent) {
			// restore "presented-after" element
			//
			BreakdownElement prev =  (BreakdownElement) activity.getBreakdownElements().get(transferLocation);		
			BreakdownElement e = (BreakdownElement) elementObj;
			e.setPresentedAfter(prev.getPresentedAfter());
			prev.setPresentedAfter(e);
		}
	}

	protected boolean prepare() {
		return true;
	}

	public Collection getModifiedResources() {
		return modifiedResources;
	}
}
