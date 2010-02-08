package org.eclipse.epf.library.realization;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.epf.library.edit.realization.IRealizationManager;
import org.eclipse.epf.library.edit.realization.RealizationContext;
import org.eclipse.epf.library.realization.impl.RealizationManager;

public class RealizationManagerFactory {
	
	public static RealizationManagerFactory instance = new RealizationManagerFactory();
	
	private Map<String, CachedMgr> map = new HashMap<String, CachedMgr>();

	public static RealizationManagerFactory getInstance() {
		return instance;
	}

	public RealizationManagerFactory() {		
	}
	
	public IRealizationManager beginUsingRealizationManager(RealizationContext context, Object caller) {
		CachedMgr cachedMgr = map.get(context.getId());
		if (cachedMgr == null) {
			cachedMgr = new CachedMgr();
			map.put(context.getId(), cachedMgr);
			cachedMgr.mgr =  new RealizationManager(context);
		}
		cachedMgr.contextCount++;
		if (IRealizationManager.debug) {
			System.out.println("LD> beginUsingRealizationManager, context: " + context); //$NON-NLS-1$
			System.out.println("LD> cachedMgr.contextCount: " + cachedMgr.contextCount); //$NON-NLS-1$
			System.out.println("LD> caller: " + caller); //$NON-NLS-1$
			System.out.println(""); //$NON-NLS-1$
		}
		return cachedMgr.mgr;
	}
	
	public void endUsingRealizationManager(RealizationContext context, Object caller) {
		CachedMgr cachedMgr = map.get(context.getId());
		if (IRealizationManager.debug) {
			System.out.println("LD> endUsingRealizationManager, context: " //$NON-NLS-1$
					+ context);
			System.out.println("LD> cachedMgr.contextCount: " //$NON-NLS-1$
					+ (cachedMgr == null ? "0" : cachedMgr.contextCount)); //$NON-NLS-1$
			System.out.println("LD> caller: " + caller); //$NON-NLS-1$
			System.out.println(""); //$NON-NLS-1$
		}
		if (cachedMgr != null) {
			cachedMgr.contextCount--;
			if (cachedMgr.contextCount == 0) {
				map.remove(context.getId());
				cachedMgr.mgr.dispose();
			}
		}
	}
	
	class CachedMgr {
		RealizationManager mgr;
		int contextCount = 0;
	}
	
}
