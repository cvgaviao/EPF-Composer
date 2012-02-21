package org.eclipse.epf.library.edit.meta;

import org.w3c.dom.Element;

public interface IMetaDef {
	
	public static final String scopeSeperator = ".."; 							//$NON-NLS-1$

	public static final String MODIFIED_TYPE = "modifiedType"; 					//$NON-NLS-1$
	
	public static final String SECTION = "section"; 						//$NON-NLS-1$
	
	public static final String REFERENCE = "reference"; 						//$NON-NLS-1$
	
	public static final String RTE = "rte"; 									//$NON-NLS-1$
	
	//Old
	public static final String NAME = "name"; 									//$NON-NLS-1$

	public static final String ID = "id"; 										//$NON-NLS-1$
	
	public static final String SUPPRESSED = "suppressed"; 						//$NON-NLS-1$
	
	public static final String type = "type"; 									//$NON-NLS-1$
	
	public static final String REFERENCE_QUALIFIERS = "reference_qualifiers"; 	//$NON-NLS-1$

	public static final String QUALIFIER = "qualifier"; 						//$NON-NLS-1$
	
	public String getName();
	
	public void parseElement(Element element)	throws TypeDefException;
		
}
