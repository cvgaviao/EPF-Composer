package org.eclipse.epf.library.realization.impl;

import org.eclipse.epf.library.realization.IRealizedTaskDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;

public class RealizedTaskDescriptor extends RealizedDescriptor implements
		IRealizedTaskDescriptor {

	public RealizedTaskDescriptor(TaskDescriptor td) {
		super(td);
	}
	
}
