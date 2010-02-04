package org.eclipse.epf.library.realization;

import java.util.List;

import org.eclipse.epf.uma.RoleDescriptor;

public interface IRealizedTaskDescriptor  extends IRealizedDescriptor {

	List<RoleDescriptor> getPerformedPrimarilyBy();
	
}
