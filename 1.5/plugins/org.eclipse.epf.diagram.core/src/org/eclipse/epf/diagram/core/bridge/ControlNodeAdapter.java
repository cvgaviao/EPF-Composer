//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.diagram.core.bridge;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkBreakdownElement;
import org.eclipse.epf.uma.WorkOrder;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.uml2.uml.ActivityEdge;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ControlNode;

/**
 * @author Phong Nguyen Le
 *
 * @since 1.2
 */
public class ControlNodeAdapter extends NodeAdapter {

	protected boolean addToUMA(ActivityEdge link) {
		if (!super.addToUMA(link))
			return false;

		MethodElement srcElement = BridgeHelper.getMethodElement(link.getSource());
		if (srcElement instanceof WorkBreakdownElement) {
			if (BridgeHelper.isSynchBar(link.getTarget())) {
				Collection targetActNodes = new ArrayList();
				BridgeHelper.getSyncBarTargetNodes(link
						.getTarget(), targetActNodes);
				for (Iterator iter = targetActNodes.iterator(); iter.hasNext();) {
					ActivityNode node = (ActivityNode) iter.next();
					WorkBreakdownElement targetElement = (WorkBreakdownElement) BridgeHelper.getMethodElement(node);
					if (UmaUtil.findWorkOrder(targetElement, srcElement) == null) {
						addDefaultWorkOrder(node, (WorkBreakdownElement) srcElement);
					}
				}
			}
		} else if (BridgeHelper.isSynchBar(link.getSource())) {
			// Predecessor should be created only in case of
			// Syncronization Bar, not for DecisionPoint.
			Collection srcActNodes = new ArrayList();
			BridgeHelper.getSyncBarSourceNodes(link.getSource(), srcActNodes);
			Collection targetActNodes = new ArrayList();
			BridgeHelper.getSyncBarTargetNodes(link.getTarget(), targetActNodes);

			for (Iterator iter = targetActNodes.iterator(); iter.hasNext();) {
				ActivityNode node = ((ActivityNode) iter.next());
				for (Iterator iterator = srcActNodes.iterator(); iterator
				.hasNext();) {
					WorkBreakdownElement pred = (WorkBreakdownElement) BridgeHelper
						.getMethodElement((ActivityNode) iterator.next());
					if (UmaUtil.findWorkOrder((WorkBreakdownElement) BridgeHelper
							.getMethodElement(node), pred) == null) {
						addDefaultWorkOrder(node, pred);
					}
				}
			}
		}

		return true;
	}
	
	protected void removeFromUMA(ActivityEdge link, ActivityNode oldSource, ActivityNode oldTarget) {
		Collection srcActNodes = new ArrayList();			
		BridgeHelper.getSourceNodes(srcActNodes, oldSource, WorkBreakdownElement.class);
		MethodElement targetElement = BridgeHelper.getMethodElement(oldTarget);
		if (targetElement instanceof WorkBreakdownElement) {
			WorkBreakdownElement targetWbe = (WorkBreakdownElement) targetElement;
			for (Iterator iterator = srcActNodes.iterator(); iterator.hasNext();) {
				// Object pred = ((Node) iterator.next()).getObject();
				// GraphicalDataHelper.removeWorkOrder((NamedNodeImpl)
				// oldTarget, pred);
				ActivityNode node = (ActivityNode) iterator.next();
				if (BridgeHelper.canRemoveAllPreds(link, node, oldTarget)) {
					MethodElement srcElement = BridgeHelper.getMethodElement(node);					
					WorkOrder wo;
					while ((wo = UmaUtil.findWorkOrder(targetWbe, srcElement)) != null) {
						getActionManager().doAction(IActionManager.REMOVE, targetWbe, 
								UmaPackage.Literals.WORK_BREAKDOWN_ELEMENT__LINK_TO_PREDECESSOR, wo, -1);
					}
				}
			}
		} else if (oldTarget instanceof ControlNode) {
			Collection targetActNodes = new ArrayList();
			BridgeHelper.getTargetNodes(targetActNodes,
					oldTarget, WorkBreakdownElement.class);

			// remove the work orders of target activities that have the
			// predecessor in srcActNodes
			//
			for (Iterator iter = targetActNodes.iterator(); iter.hasNext();) {
				ActivityNode node = ((ActivityNode) iter.next());
				for (Iterator iterator = srcActNodes.iterator(); iterator
						.hasNext();) {
					// Object pred = ((Node) iterator.next()).getObject();
					// GraphicalDataHelper.removeWorkOrder(node, pred);
					ActivityNode prednode = (ActivityNode) iterator.next();
					if (BridgeHelper.canRemoveAllPreds(link, prednode,
							node)) {
						WorkBreakdownElement targetE = (WorkBreakdownElement) BridgeHelper.getMethodElement(node);
						MethodElement srcElement = BridgeHelper.getMethodElement(prednode);
						WorkOrder wo;
						while ((wo = UmaUtil.findWorkOrder(targetE, srcElement)) != null) {
							getActionManager().doAction(IActionManager.REMOVE, targetE, 
									UmaPackage.Literals.WORK_BREAKDOWN_ELEMENT__LINK_TO_PREDECESSOR, wo, -1);
						}
					}
				}
			}
		}

		super.removeFromUMA(link, oldSource, oldTarget);
	}
}
