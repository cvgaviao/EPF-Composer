package org.eclipse.epf.uma.util;

import org.eclipse.epf.uma.ecore.IUserDefinedTypeMeta;


public interface MetaElement extends IUserDefinedTypeMeta {

	public static final MetaElement noneValue = new MetaElement() {
		public String getId() {
			return null;
		}
		public void setId(String id) {			
		}
		public String getName() {
			return null;
		}
		public void setName(String name) {			
		}
	};
		
	
	public String getId();
	public void setId(String id);
	
	public String getName();
	public void setName(String name);	
	
}
