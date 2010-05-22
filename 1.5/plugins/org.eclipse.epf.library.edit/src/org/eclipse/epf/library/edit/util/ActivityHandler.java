//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.edit.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.common.command.CompoundCommand;
import org.eclipse.emf.common.command.UnexecutableCommand;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.command.CommandParameter;
import org.eclipse.emf.edit.command.CopyCommand;
import org.eclipse.emf.edit.command.CopyCommand.Helper;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.provider.AdapterFactoryTreeIterator;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.epf.library.edit.IConfigurator;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.process.IBSItemProvider;
import org.eclipse.epf.library.edit.process.command.ActivityDeepCopyCommand;
import org.eclipse.epf.library.edit.process.command.CopyHelper;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessPackage;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.VariabilityType;
import org.eclipse.epf.uma.WorkBreakdownElement;
import org.eclipse.epf.uma.edit.domain.TraceableAdapterFactoryEditingDomain;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 * Used to extend/copy activities
 * 
 * @author Phong Nguyen Le
 * @since 1.0
 */
public class ActivityHandler {

	private ArrayList<ProcessPackage> procPackages;
	
	/**
	 * List of activities or activity wrappers to deep copy.
	 */
	private ArrayList<Object> activitiesToDeepCopy;	
	
	private MethodConfiguration deepCopyConfig;	
	private List deepCopies;
	private Map deepCopyToOriginalMap;
	private ArrayList<Activity> activities;

	private Map copyToOriginal;
	
	private AdapterFactoryEditingDomain editingDomain;
	
	private Helper copyHelper;
	
	private CopyHelper deepCopyHelper;

	private Process targetProcess;
	
	private IProgressMonitor monitor;
	
	private IConfigurator activityDeepCopyConfigurator;
	
	private boolean copyExternalVariations = true;

	/**
	 * Constructs a new ActivityHandler
	 */
	public ActivityHandler() {
		activities = new ArrayList();
		activitiesToDeepCopy = new ArrayList<Object>();
		deepCopies = new ArrayList();
		deepCopyToOriginalMap = new HashMap();
		copyToOriginal = new HashMap();
		procPackages = new ArrayList();	
		copyHelper = new Helper();
	}
	
	public Map<Object, Object> cloneOrignaltoCopyMap() {
		Map oMap = null;
		if (deepCopyToOriginalMap != null && !deepCopyToOriginalMap.isEmpty()) {
			oMap = getDeepCopyHelper().getObjectToCopyMap();
		} else if (editingDomain instanceof TraceableAdapterFactoryEditingDomain) {
			TraceableAdapterFactoryEditingDomain td = (TraceableAdapterFactoryEditingDomain) editingDomain;
			oMap = copy(td.getOriginalToClipboardMap());
		}
		return oMap;
	}
	
	public static Map copy(Map map) {
		if (map == null) {
			return null;
		}
		Map ret = new HashMap();		
		for (Map.Entry entry: (Set<Map.Entry>) map.entrySet()) {
			Object key = entry.getKey();
			Object val = entry.getValue();
			ret.put(key, val);
		}
		return ret;
	}
	
	public void dispose() {
		activities.clear();
		activitiesToDeepCopy.clear();
		copyToOriginal.clear();
		procPackages.clear();
		copyHelper.clear();		
		deepCopies.clear();		
		if(deepCopyHelper != null) {
			deepCopyHelper.clear();
		}
		deepCopyToOriginalMap.clear();
	}
	
	public void setCopyExternalVariations(boolean copyExternalVariations) {
		this.copyExternalVariations = copyExternalVariations;
	}
	
	public void copy(Activity activity) {
		procPackages.add((ProcessPackage) activity.eContainer());
	}
	
	public MethodConfiguration getDeepCopyConfig() {
		return deepCopyConfig;
	}

	public void setDeepCopyConfig(MethodConfiguration deepCopyConfig) {
		this.deepCopyConfig = deepCopyConfig;
	}
	
	public Process getTargetProcess() {
		return targetProcess;
	}

	public void setTargetProcess(org.eclipse.epf.uma.Process proc) {
		targetProcess = proc;
	}
	
	public void setMonitor(IProgressMonitor monitor) {
		this.monitor = monitor;
	}

	public List getDeepCopies() {
		return deepCopies;
	}

	public void deepCopy(Object activityOrWrapper) {
		Object unwrapped = TngUtil.unwrap(activityOrWrapper);
		if(unwrapped instanceof Activity && ((Activity)unwrapped).eContainer() != null) {
			activitiesToDeepCopy.add(activityOrWrapper);
		}
	}	
	
	public void extend(Activity act) {
		Activity extendedAct = ProcessUtil.generalize(act,
				VariabilityType.EXTENDS);
		activities.add(extendedAct);
	}
	
	public List<Activity> getActivities() {
		if (!procPackages.isEmpty() || !activitiesToDeepCopy.isEmpty()) {
			editingDomain = new TraceableAdapterFactoryEditingDomain(
					TngAdapterFactory.INSTANCE.getWBS_ComposedAdapterFactory(),
					new BasicCommandStack());
			if(!procPackages.isEmpty()) {
				activities.addAll(copy(procPackages));
				fixGuidReferences(copyHelper);
			}
			if(!activitiesToDeepCopy.isEmpty()) {
					if (monitor == null) {
					monitor = new NullProgressMonitor();
				}
				for (Object act : activitiesToDeepCopy) {
					ActivityDeepCopyCommand cmd = new ActivityDeepCopyCommand(
							act, getDeepCopyHelper(), deepCopyConfig, targetProcess, monitor,activityDeepCopyConfigurator);
					cmd.setCopyExternalVariations(copyExternalVariations);
					try {
						long time = 0;
						if (TngUtil.DEBUG) {
							time = System.currentTimeMillis();
						}
						cmd.execute();
						if (TngUtil.DEBUG) {
							System.out
									.println("ActivityDeepCopyCommand executed: " //$NON-NLS-1$
											+ (System.currentTimeMillis() - time)
											+ " ms"); //$NON-NLS-1$
							time = System.currentTimeMillis();
						}
						Collection<?> result = cmd.getResult();
						if (!result.isEmpty()) {
							Activity deepCopy = (Activity) result.iterator()
									.next();
							ProcessUtil
									.fixBreakdonwElementOrderRecursively(deepCopy);
							if (TngUtil.DEBUG) {
								System.out
										.println("ProcessUtil.fixBreakdonwElementOrderRecursively(): " //$NON-NLS-1$
												+ (System.currentTimeMillis() - time)
												+ " ms"); //$NON-NLS-1$
								time = System.currentTimeMillis();
							}
							cmd.copySuppressionStates();
							if (TngUtil.DEBUG) {
								System.out
										.println("ActivityDeepCopyCommand.copySuppressionStates(): " //$NON-NLS-1$
												+ (System.currentTimeMillis() - time)
												+ " ms"); //$NON-NLS-1$
								time = System.currentTimeMillis();
							}
							cmd.fixReferences();
							deepCopies.add(deepCopy);
							deepCopyToOriginalMap.put(deepCopy, act);
						}
					} finally {
						cmd.dispose();
					}
					fixGuidReferences(deepCopyHelper);
				}

				activities.addAll(deepCopies);
			}
		}

		return activities;
	}
	
	private void fixGuidReferences(Map<? extends Object, ? extends Object> objectToCopyMap) {
		if (true) {	//The method is not ready, by-pass for now
			return;
		}
		if (! ProcessUtil.isSynFree()) {
			return;
		}
		Set<Descriptor> cpyDesSet = new HashSet<Descriptor>();
		Map<String, String> srcGuidToCpyGuidMap = new HashMap<String, String>();

		for (Map.Entry entry : objectToCopyMap.entrySet()) {
			Object src = entry.getKey();
			Object cpy = entry.getValue();
			if (src instanceof TaskDescriptor && cpy instanceof TaskDescriptor) {
				Descriptor srcDes = (Descriptor) src;
				Descriptor cpyDes = (Descriptor) cpy;
				cpyDesSet.add(cpyDes);
				srcGuidToCpyGuidMap.put(srcDes.getGuid(), cpyDes.getGuid());
			}
		}
		
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		for (Descriptor cpy : cpyDesSet) {
			propUtil.replaceLocalUseGuidStrings(cpy, srcGuidToCpyGuidMap);
		}
		
	}
	
	private void updatePredecessors(List workBreakdownElements) {
		// get the predessor lists of original work breakdown elements
		//
		List predecessorLists = new ArrayList();		
		Object original = copyToOriginal.get(workBreakdownElements.get(0));
		AdapterFactory adapterFactory = editingDomain.getAdapterFactory();
		IBSItemProvider bsItemProvider = (IBSItemProvider) adapterFactory.adapt(original, ITreeItemContentProvider.class);
		int firstID = bsItemProvider.getId(); 
		predecessorLists.add(bsItemProvider.getPredecessors());
		// Item provider of last work breakdown element in the subtree whose root is original activity
		IBSItemProvider lastWBIp = null;
		for(Iterator iter = new AdapterFactoryTreeIterator(adapterFactory, original, false); iter.hasNext();) {
			Object obj = iter.next();
			Object e = TngUtil.unwrap(obj);
			if(e instanceof WorkBreakdownElement) {
				IBSItemProvider bsIp = null;
				if(obj instanceof IBSItemProvider) {
					bsIp = ((IBSItemProvider)obj);
					predecessorLists.add(bsIp.getPredecessors());
				}
				else {
					Object ip = adapterFactory.adapt(obj, ITreeItemContentProvider.class);
					if(ip instanceof IBSItemProvider) {
						bsIp = ((IBSItemProvider)ip);
						predecessorLists.add(bsIp.getPredecessors());
					}
				}
				if(bsIp != null) {
					lastWBIp = bsIp;
				}
			}
		}
		int lastID = lastWBIp != null ? lastWBIp.getId() : firstID;
		
		// update predecessors
		//
		int size = workBreakdownElements.size();
		Assert.isTrue(size == predecessorLists.size());
		for (int i = 0; i < size; i++) {
			WorkBreakdownElement e = (WorkBreakdownElement) workBreakdownElements.get(i);
			e.getLinkToPredecessor().clear();
			PredecessorList predList = (PredecessorList) predecessorLists.get(i);
			for (Iterator iterator = predList.iterator(); iterator.hasNext();) {
				bsItemProvider = (IBSItemProvider) iterator.next();
				int id = bsItemProvider.getId();
				if(id >= firstID && id <= lastID) {
					WorkBreakdownElement pred = (WorkBreakdownElement) workBreakdownElements.get(id - firstID);
					e.getLinkToPredecessor().add(UmaUtil.createDefaultWorkOrder(e, pred));
				}
				else {
					// predecessor is outside of the subtree, don't select it for now
				}
			}
		}
	}
	
	private Collection<?> copyProcessPackages(Collection<ProcessPackage> procPackages) {
		Command command = createCopyCommand(editingDomain, procPackages);
		if(command != null) {
			try {
				command.execute();
				return command.getResult();
			} finally {			
				command.dispose();			
			}
		}
		return Collections.EMPTY_LIST;
	}
		
	/**
	 * This creates a command that copies the given collection of objects. If the collection contains more than one object,
	 * then a compound command will be created containing individual copy commands for each object.
	 */
	private Command createCopyCommand(final EditingDomain domain, final Collection collection)
	{
		if (collection == null || collection.isEmpty()) 
		{
			return UnexecutableCommand.INSTANCE;
		}

		CompoundCommand copyCommand = new CompoundCommand(CompoundCommand.MERGE_COMMAND_ALL);
		for (Iterator objects = collection.iterator(); objects.hasNext(); )
		{
			copyCommand.append(domain.createCommand(CopyCommand.class, new CommandParameter(objects.next(), null, copyHelper)));
		}

		return copyCommand.unwrap();
	}

	
	/**
	 * Copies the given process packages
	 * 
	 * @param editingDomain
	 * @param procPackages
	 * @return activities of the copies
	 */
	private List<Activity> copy(List<ProcessPackage> procPackages) {
		Collection<?> copyPackages = copyProcessPackages(procPackages);
		ArrayList<Activity> activities = new ArrayList<Activity>();
		for (Iterator<?> iter = copyPackages.iterator(); iter.hasNext();) {
			ProcessPackage copy = (ProcessPackage) iter.next();
			if (copy instanceof ProcessComponent) {
				Activity actCopy = ((ProcessComponent) copy)
				.getProcess();
				// copy data from ProcessComponent to a new
				// ProcessPackage
				ProcessPackage pkgCopy = UmaFactory.eINSTANCE
				.createProcessPackage();
				pkgCopy.setName(actCopy.getName());
				pkgCopy.getProcessElements().add(actCopy);
				pkgCopy.getProcessElements().addAll(
						copy.getProcessElements());
				pkgCopy.getDiagrams().addAll(copy.getDiagrams());
				pkgCopy.getChildPackages().addAll(
						copy.getChildPackages());
				
				activities.add(actCopy);
			} else {
				activities.add(ProcessUtil.findActivity(copy));
			}
		}
		return activities;
	}

	public Map getDeepCopyToOriginalMap() {
		return deepCopyToOriginalMap;
	}
	
	public Helper getCopyHelper(){
		return copyHelper;
	}
	
	public CopyHelper getDeepCopyHelper() {
		if(deepCopyHelper == null) {
			deepCopyHelper = new CopyHelper();
		}
		return deepCopyHelper;
	}

	public void setActivityDeepCopyConfigurator(
			IConfigurator activityDeepCopyConfigurator) {
		this.activityDeepCopyConfigurator = activityDeepCopyConfigurator;
	}
	
	
}
