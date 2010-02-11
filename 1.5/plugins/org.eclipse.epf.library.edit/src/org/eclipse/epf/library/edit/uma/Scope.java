package org.eclipse.epf.library.edit.uma;

import java.util.Set;

import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;


/**
 * @author Weiping Lu
 * @since 1.5.1
 */
public interface Scope extends MethodConfiguration {

	boolean inScope(MethodElement element);
	void addToScope(MethodElement element);
	void clearAll();
		
	boolean debug = true;
	
}
