package org.eclipse.epf.library.edit.util;

public class TaskDescriptorPropUtil extends DescriptorPropUtil {

	public static final String TD_PerformedPrimarilyBy_Excluded = "performedPrimarilyBy_Excluded"; 	//$NON-NLS-1$
	public static final String TD_PerformedPrimarilyBy_Extra = "performedPrimarilyBy_Extra"; 	//$NON-NLS-1$
	
	private static TaskDescriptorPropUtil taskDescriptorPropUtil = new TaskDescriptorPropUtil();
	public static DescriptorPropUtil getDesciptorPropUtil() {
		return taskDescriptorPropUtil;
	}
	
	protected TaskDescriptorPropUtil() {			
	}		
	
}
