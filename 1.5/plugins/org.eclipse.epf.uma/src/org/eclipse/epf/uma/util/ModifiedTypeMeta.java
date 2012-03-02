package org.eclipse.epf.uma.util;

import java.util.List;

public interface ModifiedTypeMeta extends MetaElement {
	
	public List<ExtendedReference> getReferences();
	public List<ExtendedAttribute> getRtes();	
	
	public List<ExtendedSection> getSections();
	public List<ExtendedSection> getReferenceSections();
	public List<ExtendedSection> getRteSections();
	public List<ExtendedTable> getTables();
	public List<String> getLinkTypes();
	
	public boolean processLink(ModifiedTypeMeta linkedMeta);
	
}
