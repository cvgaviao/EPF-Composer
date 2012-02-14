package org.eclipse.epf.uma.util;

import org.eclipse.epf.uma.ecore.IUserDefinedTypeMeta;


public interface MetaElement extends IUserDefinedTypeMeta, Comparable<MetaElement> {

	public static final MetaElement noneValue = new MetaElement() {
		public String getId() {
			return null;
		}
		public String getName() {
			return null;
		}
		public String getGlobalId() {
			return null;
		}
	    public int compareTo(MetaElement o) {
	    	return 0;
	    }
	    
		public MetaElement getParent() {
			return null;
		}

	};
		
	public String getId();	
	public String getName();
	public String getGlobalId();
	public MetaElement getParent();
	
}
