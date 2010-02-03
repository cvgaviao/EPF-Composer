package org.eclipse.epf.library.realization.impl;

import org.eclipse.epf.library.realization.IRealizedDescriptor;
import org.eclipse.epf.library.realization.IRealizedElement;
import org.eclipse.epf.uma.Descriptor;

public class RealizedDescriptor extends RealizedElement implements
		IRealizedDescriptor, IRealizedElement {

	public RealizedDescriptor(Descriptor descriptor) {
		super(descriptor);
	}
	
}
