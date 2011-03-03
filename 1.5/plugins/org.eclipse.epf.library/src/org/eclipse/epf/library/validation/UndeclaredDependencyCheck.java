package org.eclipse.epf.library.validation;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.edit.util.Misc;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.util.UmaUtil;

public class UndeclaredDependencyCheck extends ValidationAction {
	
	private Map<MethodPlugin, Set<MethodPlugin>> baseMap;
	private Map<MethodPlugin, Set<MethodPlugin>> problemPluginMap;
	private Set<MethodElement> problemElementSet;
	private boolean skipContent = true;

	public UndeclaredDependencyCheck(ValidationManager mgr) {
		super(mgr);
	}
	
	@Override
	public void run() {
		Set<MethodPlugin> pluginSet = getMgr().getPluginSet();
		if (pluginSet == null || pluginSet.isEmpty()) {
			return;
		}

		//Initialization
		problemPluginMap = new HashMap<MethodPlugin, Set<MethodPlugin>>();
		problemElementSet = new HashSet<MethodElement>();
		baseMap = new HashMap<MethodPlugin, Set<MethodPlugin>>();

		Misc.clearCachedMap();
		for (MethodPlugin plugin : pluginSet) {
			Set baseSet = new HashSet();
			baseSet.addAll(Misc.getAllBase1(plugin));
			baseMap.put(plugin, baseSet);
		}
		Misc.clearCachedMap();

		//Validation check
		Set<MethodElement> processed = new HashSet<MethodElement>();
		for (MethodPlugin plugin : pluginSet) {
			checkReferences(plugin, processed);
		}
		
		//Build problem markers
		//To do
		

		//Create markers
		for (Map.Entry<MethodPlugin, Set<MethodPlugin>> entry : problemPluginMap
				.entrySet()) {
			MethodPlugin plugin = entry.getKey();
			Set<MethodPlugin> set = entry.getValue();

			String msg0 = "Undeclared dependency from plug-in: \""
					+ plugin.getName() + "\" to plug-in: ";
			for (MethodPlugin p : set) {
				IMarker marker = getMgr().createMarker(plugin);

				String msg = msg0 + "\"" + p.getName() + "\"" + p.getName();
				try {
					marker.setAttribute(IMarker.MESSAGE, msg);
					
					marker.setAttribute(IMarker.SEVERITY,
							IMarker.SEVERITY_WARNING);
					
					marker.setAttribute(IMarker.LOCATION, TngUtil.getLabelWithPath(plugin));
					
				} catch (Exception e) {
					LibraryPlugin.getDefault().getLogger().logError(e);
				}
			}
		}

	}
	
	private void addToProblemPluginMap(MethodPlugin plugin, MethodPlugin referencedPlugin) {
		Set<MethodPlugin> set = problemPluginMap.get(plugin);
		if (set == null) {
			set = new HashSet<MethodPlugin>();
			problemPluginMap.put(plugin, set);
		}
		set.add(referencedPlugin);
	}
	
	
	private void checkReferences(MethodElement me, Set<MethodElement> processed) {
		if (processed.contains(me)) {
			return;
		}
		processed.add(me);
		MethodPlugin ownerPlugin = UmaUtil.getMethodPlugin(me);
		if ( ! getMgr().getPluginSet().contains(ownerPlugin)) {
			return;
		}
		
		Set<MethodPlugin> baseSet = baseMap.get(ownerPlugin);
				
		EList<EReference> refList = me.eClass().getEAllReferences();
		if (refList == null || refList.isEmpty()) {
			return;
		}
		for (EReference ref : refList) {
			if (skipContent && LibraryUtil.isContentRef(ref)) {
				continue;
			}
			Object obj = me.eGet(ref);
			if (obj instanceof MethodElement) {
				MethodElement referenced = (MethodElement) obj;
				recordOneHit(me, ownerPlugin, baseSet, referenced);				
				checkReferences(referenced, processed);
				
			} else if (obj instanceof List) {
				List list = (List) obj;
				for (Object itemObj : list) {
					if (itemObj instanceof MethodElement) {
						MethodElement referenced = (MethodElement) itemObj;					
						recordOneHit(me, ownerPlugin, baseSet, referenced);	
						checkReferences(referenced, processed);
					}
				}
			}
		}
	}

	private void recordOneHit(MethodElement me, MethodPlugin ownerPlugin,
			Set<MethodPlugin> baseSet, MethodElement referenced) {
		MethodPlugin referencedPlugin = UmaUtil.getMethodPlugin(referenced);
		if (referencedPlugin != null && referencedPlugin != ownerPlugin && !baseSet.contains(referencedPlugin)) {
			addToProblemPluginMap(ownerPlugin, referencedPlugin);
			problemElementSet.add(me);
		}
	}
		
	public void clearResults() {		
	}

	public void addPluginFix(IMarker marker) {
		
	}
	
	public void removeReferenceFix(IMarker marker) {
		
	}
	
}
