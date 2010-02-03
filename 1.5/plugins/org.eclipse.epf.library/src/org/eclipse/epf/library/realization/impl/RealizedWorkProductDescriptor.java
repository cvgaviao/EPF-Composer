package org.eclipse.epf.library.realization.impl;

import org.eclipse.epf.library.realization.IRealizedWorkProductDescriptor;
import org.eclipse.epf.uma.WorkProductDescriptor;

public class RealizedWorkProductDescriptor extends
		RealizedDescriptor implements IRealizedWorkProductDescriptor {

	public RealizedWorkProductDescriptor(WorkProductDescriptor wpd) {
		super(wpd);
	}
	
	
}
