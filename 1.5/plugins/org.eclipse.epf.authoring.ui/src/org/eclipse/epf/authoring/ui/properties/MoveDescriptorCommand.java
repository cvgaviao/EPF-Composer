/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.authoring.ui.properties;

import java.util.List;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.uma.Descriptor;

/**
 * 
 * @author achen
 *
 */
public class MoveDescriptorCommand extends RemoveDescriptorCommand {
	private Descriptor desc;
	private List items;
	private List methodElements;
	private int toFeature;
	
	public MoveDescriptorCommand(Descriptor desc, List items, int fromFeature, int toFeature) {
		super(desc, items, fromFeature, false);
		
		this.desc = desc;
		this.items = items;
		methodElements = ProcessUtil.getAssociatedElementList(items);
		this.toFeature = toFeature;
	}
	
	public void execute() {
		redo();
	}

	public void redo() {
		super.redo();		
		EStructuralFeature feature = desc.eClass().getEStructuralFeature(toFeature);		
		Object value = desc.eGet(feature);
		if (value instanceof List) {
			List listValue = (List) value;
			listValue.addAll(methodElements);
		}			
	}
	
	public void undo() {
		super.undo();		
		EStructuralFeature feature = desc.eClass().getEStructuralFeature(toFeature);		
		Object value = desc.eGet(feature);
		if (value instanceof List) {
			List listValue = (List) value;
			listValue.removeAll(methodElements);
		}
	}

}
