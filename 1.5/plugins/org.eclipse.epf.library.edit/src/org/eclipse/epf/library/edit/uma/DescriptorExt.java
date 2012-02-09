package org.eclipse.epf.library.edit.uma;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject.ExtendObject;
import org.eclipse.epf.uma.util.UmaUtil;

import org.eclipse.epf.uma.Descriptor;

public class DescriptorExt extends MethodElementExt {

	private Descriptor greenParent;
	private Set<Descriptor> customizingChildSet;

	public DescriptorExt (Descriptor des, ExtendObject oldOb) {
		super(des, oldOb);
	}
	
	@Override
	protected void copy(ExtendObject oldObj) {
		super.copy(oldObj);
		if (! (oldObj instanceof DescriptorExt)) {
			return;
		}
		DescriptorExt old = (DescriptorExt) oldObj;
		
		greenParent = 			old.greenParent;
		customizingChildSet = 	old.customizingChildSet;
	}
	
	public void setGreenParent(Descriptor greenParent) {
		this.greenParent = greenParent;
	}
	
	public Descriptor getGreenParent() {
		return greenParent;
	}
	
	public Descriptor getDescriptor() {
		return (Descriptor) getElement();
	}
	
	public List<? extends Descriptor> getCustomizingChildren() {		
		List<Descriptor> children = new ArrayList<Descriptor>();
		List<Descriptor> toRemoveList = null;
		if (customizingChildSet != null && !customizingChildSet.isEmpty()) {
			for (Descriptor des : customizingChildSet) {
				if (UmaUtil.isInLibrary(des)) {
					children.add(des);
				} else {
					if (toRemoveList == null) {
						toRemoveList = new ArrayList<Descriptor>(); 
					}
					toRemoveList.add(des);
				}
			}
			if (toRemoveList != null) {
				customizingChildSet.removeAll(toRemoveList);
			}
		}
		return children;
	}
	
	public void addToCustomizingChildren(Descriptor child) {
		if (child == null) {
			return;
		}
		if (customizingChildSet == null) {
			customizingChildSet = new HashSet<Descriptor>();
		}
		customizingChildSet.add(child);
	}
		
}
