package org.eclipse.epf.library.edit.realization;

import java.util.Set;

import org.eclipse.epf.uma.Descriptor;


public interface IRealizedDescriptor extends IRealizedElement {
	
	Set<Descriptor> getAllReferenced();
	
}
