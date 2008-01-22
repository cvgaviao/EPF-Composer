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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.provider.AdapterFactoryTreeIterator;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.LibraryEditResources;
import org.eclipse.epf.library.edit.process.BreakdownElementWrapperItemProvider;
import org.eclipse.epf.library.edit.process.IBSItemProvider;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.Milestone;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.VariabilityElement;
import org.eclipse.epf.uma.WorkBreakdownElement;
import org.eclipse.epf.uma.WorkOrder;
import org.eclipse.epf.uma.WorkOrderType;

/**
 * This class represents a predecessor list of an item provider for a work
 * breakdown element. It calculates the predecessors based on the currently
 * selected configuration and according to variability rules. It can refresh
 * itself upon change in predecessor list of work breakdown element.
 * 
 * @author Phong Nguyen Le
 * @since 1.0
 */
public class PredecessorList extends ArrayList<Object> {
	/**
	 * Comment for <code>serialVersionUID</code>
	 */
	private static final long serialVersionUID = 3617853092570412082L;	
	
	private static final String FINISH_TO_START = LibraryEditResources.WorkOrderTypeAbbreviation_FINISH_TO_START; 
	private static final String FINISH_TO_FINISH = LibraryEditResources.WorkOrderTypeAbbreviation_FINISH_TO_FINISH; 
	private static final String START_TO_START = LibraryEditResources.WorkOrderTypeAbbreviation_START_TO_START; 
	private static final String START_TO_FINISH = LibraryEditResources.WorkOrderTypeAbbreviation_START_TO_FINISH; 
	private Map map4LinkType = new HashMap();

	public static final PredecessorList EMPTY_LIST = new PredecessorList() {

		/**
		 * Comment for <code>serialVersionUID</code>
		 */
		private static final long serialVersionUID = 3904676098217097016L;

		public void refresh() {

		}

		public void add(int index, Object element) {

		}

		public boolean add(Object o) {
			throw new UnsupportedOperationException();
		}

		public boolean addAll(java.util.Collection c) {
			throw new UnsupportedOperationException();
		}

		public boolean addAll(int index, java.util.Collection c) {
			throw new UnsupportedOperationException();
		}

	};

	private AdapterFactory adapterFactory;

	private Object object;

	private Adapter listener = new AdapterImpl() {
		public void notifyChanged(org.eclipse.emf.common.notify.Notification msg) {
			switch (msg.getFeatureID(BreakdownElement.class)) {
			case UmaPackage.WORK_BREAKDOWN_ELEMENT__LINK_TO_PREDECESSOR:
				refresh();
				return;
			}
		}
	};

	private Object top;

	private PredecessorList() {

	}

	public PredecessorList(AdapterFactory adapterFactory, Object object) {
		this.adapterFactory = adapterFactory;
		this.object = object;
		BreakdownElement e = (BreakdownElement) TngUtil.unwrap(object);
		e.eAdapters().add(0, listener);
		if (!map4LinkType.isEmpty())
			map4LinkType.clear();
		initialize();
	}

	public void dispose() {
		Object e = TngUtil.unwrap(object);
		if (e instanceof EObject) {
			((EObject) e).eAdapters().remove(listener);
		}
		clear();
	}

	/**
	 * Gets the right top item in the breakdown structure tree to search for
	 * item providers of predecessors
	 * 
	 * @return
	 */
	protected Object getTopItem() {
		if (object instanceof BreakdownElementWrapperItemProvider) {
			BreakdownElementWrapperItemProvider itemProvider = (BreakdownElementWrapperItemProvider) object;
			if (itemProvider.isReadOnly()) {
				// this represents an inherited breakdown element
				//
				BreakdownElement e = (BreakdownElement) TngUtil.unwrap(object);
				Process proc = TngUtil.getOwningProcess(e);

				Object top = itemProvider;
				for (Object parent = itemProvider.getParent(object); parent != null;) {
					top = parent;
					BreakdownElement parentElement = (BreakdownElement) TngUtil
							.unwrap(parent);
					Process parentProc = TngUtil
							.getOwningProcess(parentElement);
					if (parentProc != proc) {
						break;
					}
					if (parent instanceof ITreeItemContentProvider) {
						parent = ((ITreeItemContentProvider) parent)
								.getParent(parent);
					} else {
						ITreeItemContentProvider adapter = (ITreeItemContentProvider) adapterFactory
								.adapt(parent, ITreeItemContentProvider.class);
						parent = adapter.getParent(parent);
					}
				}
				return top;
			} else {
				return itemProvider.getTopItem();
			}
		} else {
			IBSItemProvider adapter = (IBSItemProvider) adapterFactory.adapt(
					object, ITreeItemContentProvider.class);
			return adapter.getTopItem();
		}
	}
	
	public static class DepthLevelItemProvider {
		private int depthLevel;
		private Object itemProvider;
		private Object object;
		private Object element;
		
		private DepthLevelItemProvider(Object object, int depthLevel, Object itemProvider, Object element) {
			super();
			this.object = object;
			this.depthLevel = depthLevel;
			this.itemProvider = itemProvider;
			this.element = element;
		}	
		
		public Object getItemProvider() {
			return itemProvider;
		}
	}
	
	static List<DepthLevelItemProvider> createItemProviderList(Object top, AdapterFactory adapterFactory) {
		List<DepthLevelItemProvider> list = new ArrayList<DepthLevelItemProvider>();
		DepthLevelAdapterFactoryTreeIterator<Object> iter = new DepthLevelAdapterFactoryTreeIterator<Object>(
				adapterFactory, top) {
					private static final long serialVersionUID = 1L;
					@Override
					protected Iterator<Object> getChildren(Object o) {
						Object e = TngUtil.unwrap(o); 
						if(e instanceof Descriptor || e instanceof Milestone) {
							return Collections.emptyList().iterator();
						}
						return super.getChildren(o);
					}			
		};
		for(; iter.hasNext();) {
			Object object = iter.next();
			Object element = TngUtil.unwrap(object);
			if(element instanceof WorkBreakdownElement) {
				int depthLevel = iter.getDepthLevel();
				Object itemProvider = adapterFactory.adapt(object, ITreeItemContentProvider.class);
				list.add(new DepthLevelItemProvider(object, depthLevel, itemProvider, element));
			}
		}
		return list;
	}
	
	private static void updateElementToItemProvidersMap(Map<Object, Collection<Object>> map, Object element, Object itemProvider) {
		Collection<Object> itemProviders = map.get(element);
		if(itemProviders == null) {
			itemProviders = new ArrayList<Object>();
			map.put(element, itemProviders);
		}
		itemProviders.add(itemProvider);
		if (element instanceof VariabilityElement) {
			VariabilityElement ve = (VariabilityElement) element;
			if (ve.getVariabilityBasedOnElement() != null) {
				itemProviders = map.get(ve.getVariabilityBasedOnElement());
				if(itemProviders == null) {
					itemProviders = new ArrayList<Object>();
					map.put(ve.getVariabilityBasedOnElement(), itemProviders);
				}
				itemProviders.add(itemProvider);
			}
		}
	}
	
	Map<?, Collection<Object>> createBreakdownElementToItemProviderMap(List<DepthLevelItemProvider> list) {
		Object top = getTopItem();
		// find top's item provider
		//
		DepthLevelItemProvider topIp = null;
		int startIndex = 0;
		for (DepthLevelItemProvider itemProvider : list) {
			if(itemProvider.object == top) {
				topIp = itemProvider;
				break;
			}
			else {
				startIndex++;
			}
		}
		assert topIp != null : "Could not find item provider of top object in the given item provider list."; //$NON-NLS-1$
		// find the index of last item provider in the subtree of top
		//
		int size = list.size();
		int lastIndex = startIndex + 1;
		for (; lastIndex < size && list.get(lastIndex).depthLevel > topIp.depthLevel; lastIndex++);		
			
		// copy the tree to a map of breakdown element / item provider for
		// reuse
		//
		HashMap<Object, Collection<Object>> map = new HashMap<Object, Collection<Object>>();
		for (DepthLevelItemProvider itemProvider : list.subList(startIndex, lastIndex)) {
			updateElementToItemProvidersMap(map, itemProvider.element, itemProvider.itemProvider);
		}
		return map;
	}
	
	static Map<Object, Collection<Object>> createBreakdownElementToItemProviderMap(Object top, AdapterFactory adapterFactory) {
		AdapterFactoryTreeIterator<Object> iter = new AdapterFactoryTreeIterator<Object>(
				adapterFactory, top) {
			private static final long serialVersionUID = 1L;
			@Override
			protected Iterator<Object> getChildren(Object o) {
				Object e = TngUtil.unwrap(o); 
				if(e instanceof Descriptor || e instanceof Milestone) {
					return Collections.emptyList().iterator();
				}
				return super.getChildren(o);
			}			
		};
		
		// copy the tree to a map of breakdown element / item provider for
		// reuse
		//
		HashMap<Object, Collection<Object>> map = new HashMap<Object, Collection<Object>>();
		while (iter.hasNext()) {
			Object obj = iter.next();
			Object be = TngUtil.unwrap(obj);
			if(be instanceof WorkBreakdownElement) {
				IBSItemProvider ip = (IBSItemProvider) (obj instanceof IBSItemProvider ? obj :
					adapterFactory.adapt(obj, ITreeItemContentProvider.class));
				updateElementToItemProvidersMap(map, be, ip);
			}
		}
		return map;
	}
	
	private void initialize() {
		Object unwrapped = TngUtil.unwrap(object);
		if (unwrapped instanceof WorkBreakdownElement) {
			WorkBreakdownElement e = (WorkBreakdownElement) unwrapped;
			List workOrders = e.getLinkToPredecessor();
			if (workOrders.isEmpty())
				return;
			top = getTopItem();	
			initialize(createBreakdownElementToItemProviderMap(top, adapterFactory));
		}
	}
	
	private void initialize(Map<?, Collection<Object>> map) {

		if (TngUtil.unwrap(object) instanceof WorkBreakdownElement) {
			WorkBreakdownElement e = (WorkBreakdownElement) TngUtil
					.unwrap(object);
			List workOrders = e.getLinkToPredecessor();
			if (workOrders.isEmpty())
				return;
			WorkBreakdownElement topElement = (WorkBreakdownElement) TngUtil.unwrap(top);
			String topGUID = topElement.getGuid();
			
			int n = workOrders.size();
			for (int i = 0; i < n; i++) {
				WorkOrder workOrder = (WorkOrder) workOrders.get(i);					

				//System.out.println(workOrder.getLinkType().getLiteral());

				BreakdownElement pred = workOrder.getPred();
				Collection<Object> itemProviders = map.get(pred);
				if(itemProviders != null && !itemProviders.isEmpty()) {
					IBSItemProvider bsItemProvider = null;
					MethodElementProperty prop = MethodElementPropertyHelper.getProperty(workOrder, 
							MethodElementPropertyHelper.WORK_ORDER__PREDECESSOR_PROCESS_PATH);
					if(prop == null) {
						// predecessor is local or WorkOrder is created before support for 'green' predecessor
						// by saving its process path in methodElementProperty of the work order
						//
						bsItemProvider = (IBSItemProvider) itemProviders.iterator().next();
					}
					else {
						String procPath = prop.getValue();
						
						// calculate the path in this process
						//						
						int index;
						String guid = topGUID;
						VariabilityElement base = topElement instanceof VariabilityElement ? ((VariabilityElement)topElement).getVariabilityBasedOnElement() : null;
						for(index = procPath.indexOf(guid); index == -1 && base != null; index = procPath.indexOf(guid)) {
							if(base != null) {
								guid = base.getGuid(); 
								base = base.getVariabilityBasedOnElement();
							}
						}
						if(index != -1) {
							StringBuffer strBuff = new StringBuffer(procPath.substring(index + guid.length()));
							Object topObject = top;
							for(WorkBreakdownElement wbe = topElement; wbe != null;) {
								strBuff.insert(0, wbe.getGuid());
								strBuff.insert(0, '/');
								if(topObject instanceof WorkBreakdownElement) {
									topObject = wbe = ((WorkBreakdownElement)topObject).getSuperActivities();
								}
								else {
									topObject = adapterFactory.adapt(topObject, ITreeItemContentProvider.class);
									wbe = (WorkBreakdownElement) TngUtil.unwrap(topObject);
								}
							}
							strBuff.insert(0, ":/").insert(0, Suppression.WBS); //$NON-NLS-1$
							String path = strBuff.toString();
							find_wrapper:
							for (Object itemProvider : itemProviders) {
								if(itemProvider instanceof BreakdownElementWrapperItemProvider) {
									String p = Suppression.getPath((BreakdownElementWrapperItemProvider) itemProvider);
									if(path.equals(p)) {
										bsItemProvider = (IBSItemProvider) itemProvider;
										break find_wrapper;
									}
								}
							}
						}
					}
					if (bsItemProvider != null) {
						add(bsItemProvider);
						map4LinkType.put(bsItemProvider.getId(), workOrder.getLinkType().getValue());
					}
				}
			}
		}
	}
	
	@Override
	public void clear() {
		super.clear();
		if (!map4LinkType.isEmpty())
			map4LinkType.clear();
	}

	protected void refresh() {
		clear();
		initialize();
	}
	
	void refresh(List<DepthLevelItemProvider> list) {
		clear();
		initialize(createBreakdownElementToItemProviderMap(list));
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.AbstractCollection#toString()
	 */
	public String toString() {
		return toUnSuppressedString(null);
	}

	public String toUnSuppressedString(Suppression sup) {
		// remove the item providers of the deleted elements
		//
		for (Iterator iter = iterator(); iter.hasNext();) {
			Object e = TngUtil.unwrap(iter.next());
			if (e instanceof ItemProviderAdapter) {
				e = ((ItemProviderAdapter) e).getTarget();
				if (e == null) {
					// object deleted
					//
					iter.remove();
				}
			}
			if (e instanceof BreakdownElement) {
				BreakdownElement be = (BreakdownElement) e;
				Activity superAct = be.getSuperActivities();
				if (superAct == null && TngUtil.getOwningProcess(be) != be) {
					iter.remove();
				}
			}
		}

		if (isEmpty())
			return ""; //$NON-NLS-1$

		StringBuffer strBuf = new StringBuffer();
		int n = size() - 1;
		for (int i = 0; i < n; i++) {
			IBSItemProvider bsItemProvider = (IBSItemProvider) get(i);
			if ((sup == null) || !sup.isSuppressed(bsItemProvider)) {
				strBuf.append(bsItemProvider.getId()).append(',');				
			}
		}
		IBSItemProvider bsItemProvider = (IBSItemProvider) get(n);
		if ((sup == null) || !sup.isSuppressed(bsItemProvider)) {
			strBuf.append(bsItemProvider.getId());
		}

		return strBuf.toString();
	}
	
	public String toUnSuppressedString(Suppression sup, boolean isLinkTypeRequired) {
		// remove the item providers of the deleted elements
		//
		for (Iterator iter = iterator(); iter.hasNext();) {
			Object e = TngUtil.unwrap(iter.next());
			if (e instanceof ItemProviderAdapter) {
				e = ((ItemProviderAdapter) e).getTarget();
				if (e == null) {
					// object deleted
					//
					iter.remove();
				}
			}
			if (e instanceof BreakdownElement) {
				BreakdownElement be = (BreakdownElement) e;
				Activity superAct = be.getSuperActivities();
				if (superAct == null && TngUtil.getOwningProcess(be) != be) {
					iter.remove();
				}
			}
		}

		if (isEmpty())
			return ""; //$NON-NLS-1$

		StringBuffer strBuf = new StringBuffer();
		int n = size() - 1;
		for (int i = 0; i < n; i++) {
			IBSItemProvider bsItemProvider = (IBSItemProvider) get(i);
			if ((sup == null) || !sup.isSuppressed(bsItemProvider)) {
				strBuf.append(bsItemProvider.getId());
			    Object value = map4LinkType.get(bsItemProvider.getId());
			    if (value != null && isLinkTypeRequired) {
			    	int v = Integer.parseInt(value.toString());
			    	String abbrev = toWorkOrderTypeAbbreviation(v);
			    	if(abbrev != null) {
			    		strBuf.append(abbrev);
			    	}
			    }
				strBuf.append(',');
			}
		}
		IBSItemProvider bsItemProvider = (IBSItemProvider) get(n);
		if ((sup == null) || !sup.isSuppressed(bsItemProvider)) {
			strBuf.append(bsItemProvider.getId());
			//System.out.println(map4LinkType.get(bsItemProvider.getId()));
			Object value = map4LinkType.get(bsItemProvider.getId());
		    if (value != null && isLinkTypeRequired) {
		    	int v = Integer.parseInt(value.toString());
		    	String abbrev = toWorkOrderTypeAbbreviation(v);
		    	if(abbrev != null) {
		    		strBuf.append(abbrev);
		    	}
		    }
		}

		return strBuf.toString();
	}
	
	public static boolean prepareUpdatePredecessors(AdapterFactory adapterFactory, WorkBreakdownElement wbe,
			List<Object> predecessors, List<WorkOrder> addList, List<WorkOrder> predToBeDeleted) {
		WorkOrder wo = null;
		int size = wbe.getLinkToPredecessor().size();
		
		// remove WorkOrders for predecessors that are not in predecessor list and accepted by the filter
		//
		IFilter filter = ProcessUtil.getFilter(adapterFactory);
		boolean nullFilter = filter == null;
		for (Iterator iter = wbe.getLinkToPredecessor().iterator(); iter
				.hasNext();) {
			wo = (WorkOrder) iter.next();
			if ((nullFilter || filter.accept(wo.getPred()))) {
				boolean found = false;
				find_pred:						
				for(Object pred : predecessors) {
					if(pred instanceof WorkBreakdownElement) {
						if(wo.getPred() == pred) {
							found = true;
							break find_pred;
						}
					}
					else if(pred instanceof BreakdownElementWrapperItemProvider) {
						if(isPredecessor((BreakdownElementWrapperItemProvider) pred, wo)) {
							found = true;
							break find_pred;
						}
					}
				}
				if(!found) {
					predToBeDeleted.add(wo);
				}
			}
		}

		size = predecessors.size();
		for (int i = 0; i < size; i++) {
			Object obj = predecessors.get(i);
			WorkBreakdownElement element = (WorkBreakdownElement) TngUtil.unwrap(obj);
			boolean found = false;
			BreakdownElementWrapperItemProvider wrapper = (BreakdownElementWrapperItemProvider) (obj instanceof BreakdownElementWrapperItemProvider ? obj : null);
			String procPath = null;
			find_pred: for (Iterator iterator = wbe
					.getLinkToPredecessor().iterator(); iterator
					.hasNext();) {
				wo = (WorkOrder) iterator.next();
				if (wo.getPred() == element) {
					if(wrapper != null) {
						if(isPredecessor(wrapper, wo)) {
							found = true;
							break find_pred;
						}
					}
					else {
						// the predecessor is local, no need to look for wrapper
						//
						found = true;
						break find_pred;
					}
				}
			}
			if (!found) {					
				wo = UmaFactory.eINSTANCE.createWorkOrder();
				wo.setPred(element);
				if(wrapper != null) {
					if(procPath == null) {
						procPath = Suppression.getPath(wrapper);
					}
					MethodElementPropertyHelper.setProperty(wo, MethodElementPropertyHelper.WORK_ORDER__PREDECESSOR_PROCESS_PATH, procPath);
				}
				addList.add(wo);
			}
		}
		return true;
	}
	
	/**
	 * Parses a command-separated list of predecessor indexes into lists of
	 * predecessorss to be added and deleted for update.
	 * 
	 * @param adapterFactory
	 * @param wbe
	 * @param indexList
	 *            a comma-separated list of predecessor indexes
	 * @param addList
	 * @param predToBeDeleted
	 * @return
	 */
	public static boolean prepareUpdatePredecessors(
			AdapterFactory adapterFactory, WorkBreakdownElement wbe,
			String indexList, List<WorkOrder> addList, List<WorkOrder> predToBeDeleted) {
		Process process = TngUtil.getOwningProcess(wbe);
		List<Object> predecessors = new ArrayList<Object>();
		if (ProcessUtil.checkPredecessorList(wbe, indexList, adapterFactory,
				process, predecessors) == null) {
			return prepareUpdatePredecessors(adapterFactory, wbe, predecessors, addList, predToBeDeleted);
		}
		return false;
	}

	public static String toWorkOrderTypeAbbreviation(int workOrderType) {
		switch(workOrderType) {
		case WorkOrderType.FINISH_TO_FINISH_VALUE:
			return FINISH_TO_FINISH;
		case WorkOrderType.START_TO_FINISH_VALUE:
			return START_TO_FINISH;
		case WorkOrderType.START_TO_START_VALUE:
			return START_TO_START;
		}
		return null;
	}
	
	/**
	 * Checks if the given wrapper is a predecessor defined by the given WorkOrder.
	 * 
	 * @param wrapper
	 * @param workOrder
	 * @return
	 */
	public static boolean isPredecessor(BreakdownElementWrapperItemProvider wrapper, WorkOrder workOrder) {
		MethodElementProperty prop = MethodElementPropertyHelper.getProperty(workOrder, 
				MethodElementPropertyHelper.WORK_ORDER__PREDECESSOR_PROCESS_PATH);
		if(prop == null) {
			return false;
		}
		else {
			String procPath = prop.getValue();
			String p = Suppression.getPath(wrapper);
			return procPath.equals(p);
		}
	}
}
