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
import java.util.Set;

import org.eclipse.emf.common.command.AbstractCommand;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IResourceAwareCommand;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.edit.util.WbePropUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.WorkBreakdownElement;


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
		
		if (elementObj instanceof WorkBreakdownElement) {
			handleWbeGlobalMove(activity, (WorkBreakdownElement) elementObj, true);
		}		
	}

	public static void handleWbeGlobalMove(Activity act, WorkBreakdownElement wbe, boolean up) {
		AdapterFactory aFactory = TngAdapterFactory.INSTANCE
				.getWBS_ComposedAdapterFactory();
		ItemProviderAdapter adapter = (ItemProviderAdapter) aFactory.adapt(
				act, ITreeItemContentProvider.class);
		
		Collection<?> children = adapter.getChildren(act);
		if (! (children instanceof List)) {
			return;
		}
		
		List<?> childList = (List<?>) children;
		
		WorkBreakdownElement globalPresentedAfter = act;
		for (int i = 0; i < childList.size(); i++) {
			Object child = childList.get(i);
			if (child == wbe) {
				Object prev = null;
				if (up) {
					if (i - 2 >= 0) {
						prev = TngUtil.unwrap(childList.get(i - 2));			
					}
				} else {
					if (i + 1 < childList.size()) {
						prev = TngUtil.unwrap(childList.get(i + 1));	
					}
				}
				
				if (prev instanceof WorkBreakdownElement) {
					globalPresentedAfter = (WorkBreakdownElement) prev;
				}
				
				break;
			}
		}
		
		Set<BreakdownElement> locals = new HashSet<BreakdownElement>(act.getBreakdownElements());
		
		if (locals.contains(globalPresentedAfter)) {
			return;
		}
		
		WbePropUtil propUtil = WbePropUtil.getWbePropUtil();		
		propUtil.setGlobalPresentedAfter(wbe, globalPresentedAfter);
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
