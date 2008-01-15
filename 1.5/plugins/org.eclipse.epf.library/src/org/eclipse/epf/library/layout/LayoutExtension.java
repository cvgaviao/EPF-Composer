package org.eclipse.epf.library.layout;

import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.util.Suppression;
import org.eclipse.epf.uma.Process;

/**
 * abstract base class for layout extendsion
 * @author Jinhua Xi
 *
 */
public abstract class LayoutExtension {

	protected ElementLayoutManager layoutMgr;
	
	/**
	 * default constructor
	 *
	 */
	public LayoutExtension() {
		
	}
	
	public void init(ElementLayoutManager layoutMgr) {
		this.layoutMgr = layoutMgr;
	}
	
	public abstract ElementRealizer getElementRealizer();
	
	/**
	 * Returns the content validator.
	 */
	public abstract IContentValidator getValidator();
	
	public abstract Suppression getSuppression(Process proc);


}
