//------------------------------------------------------------------------------
// Copyright (c) 2005, 2008 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.authoring.ui.properties;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.filters.DescriptorProcessFilter;
import org.eclipse.epf.authoring.ui.filters.ProcessWorkProductFilter;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.itemsfilter.FilterConstants;
import org.eclipse.epf.library.edit.process.command.AssignWPToMilestone;
import org.eclipse.epf.library.edit.process.command.IActionTypeConstants;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Milestone;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;


/**
 * Milestone - work product section
 * 
 * @author Shilpa Toraskar
 * @since 1.5
 * 
 */
public class MilestoneWorkProductSection extends RelationSection {
	private IFilter filter = null;

	private Milestone milestone;
	/**
	 * Get process work product filter
	 */
	public IFilter getFilter() {
		if (filter == null) {
			filter = new ProcessWorkProductFilter(getConfiguration(),
					null, FilterConstants.WORKPRODUCTS);
		}
		return filter;
	}


	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#createControls(org.eclipse.swt.widgets.Composite, org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	public void createControls(Composite parent,
			TabbedPropertySheetPage tabbedPropertySheetPage) {

		super.createControls(parent, tabbedPropertySheetPage);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#initContentProvider1()
	 */
	protected void initContentProvider1() {
		contentProvider = new AdapterFactoryContentProvider(getAdapterFactory()) {
			public Object[] getElements(Object object) {
				
				return getFilteredList(
						((Milestone) milestone).getRequiredResults())
						.toArray();
			}
		};
		tableViewer1.setContentProvider(contentProvider);
	}

	

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#init()
	 */
	protected void init() {
		if (getElement() instanceof Milestone) {
			milestone = (Milestone) getElement();
		}
		super.init();

		labelProvider = new AdapterFactoryLabelProvider(
				TngAdapterFactory.INSTANCE.getWBS_ComposedAdapterFactory());

		int numOfTables = 1;
		
		setTabData(PropertiesResources.RoleDescriptor_WorkProducts_SectionTitle,
				PropertiesResources.Milestone_WorkProducts_SectionDescription,
				PropertiesResources.Milestone_WorkProducts_Table1,
				null,
				null,
				null,
				FilterConstants.WORKPRODUCTS); 

		boolean[] changesAllowed = { true };
		setTableData(numOfTables, changesAllowed);
	}


	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#refresh()
	 */
	public void refresh() {
		if (getElement() instanceof Milestone) {
			milestone = (Milestone) getElement();
		}
		super.refresh();
	}


	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getDescriptorsFromProcess()
	 */
	protected List getDescriptorsFromProcess() {
		List items = new ArrayList();
		return ProcessUtil.getElementsInScope(getAdapterFactory(), milestone,
				WorkProductDescriptor.class, items);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems1(java.util.List)
	 */
	protected void addItems1(List items) {
		if (!items.isEmpty()) {
			AssignWPToMilestone cmd = new AssignWPToMilestone(
					(Milestone) element, items,
					IActionTypeConstants.ADD_REQUIRED_RESULT, getConfiguration());
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems1(java.util.List)
	 */
	protected void removeItems1(List items) {
		if (!items.isEmpty()) {
			actionMgr.doAction(IActionManager.REMOVE_MANY, milestone,
					UmaPackage.eINSTANCE.getMilestone_RequiredResults(),
					items, -1);
			
		}
	};

	private List getWorkProducts(List items) {
		List wpList = new ArrayList();
		if (!items.isEmpty()) {
			for (int i = 0; i < items.size(); i++) {
				wpList.add(((WorkProductDescriptor) items.get(i))
						.getWorkProduct());
			}
		}

		return wpList;
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements1()
	 */
	protected List getExistingElements1() {
		return getWorkProducts(((Milestone) milestone).getRequiredResults());
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getProcess()
	 */
	protected Process getProcess() {
		AdapterFactory aFactory = TngAdapterFactory.INSTANCE
				.getOBS_ComposedAdapterFactory();
		ItemProviderAdapter adapter = (ItemProviderAdapter) aFactory.adapt(
				milestone, ITreeItemContentProvider.class);
		Object obj = ProcessUtil.getRootProcess(aFactory, adapter, milestone);
		return (Process) obj;
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getFilterForDescriptors()
	 */
	protected IFilter getFilterForDescriptors() {
		return new DescriptorProcessFilter(getConfiguration()) {
			protected boolean childAccept(Object obj) {
				if (obj instanceof Activity) {
					List list = new ArrayList();
					getActivitiesInScope(TngAdapterFactory.INSTANCE
							.getWBS_ComposedAdapterFactory(), milestone, list);
					if (list.contains(obj))
						return true;
					else
						return false;
				}
				// Uncomment to show extended activity's workproducts
				// if(obj instanceof WorkProductDescriptorWrapperItemProvider){
				// Object object =
				// ((BreakdownElementWrapperItemProvider)obj).getParent(obj);
				// List list = new ArrayList();
				// getActivitiesInScope(
				// TngAdapterFactory.INSTANCE.getOBS_ComposedAdapterFactory(),
				// element, list);
				// if(list.contains(object)) return true;
				// }
				if (obj instanceof WorkProductDescriptor)
					return true;
				return false;
			}
		};
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getDescriptorTabName()
	 */
	protected String getDescriptorTabName() {
		return FilterConstants.WORK_PRODUCT_DESCRIPTORS;
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems1(java.util.List)
	 */
	protected void addFromProcessItems1(List items) {
		if (!items.isEmpty()) {
			actionMgr.doAction(IActionManager.ADD_MANY, milestone,
					UmaPackage.eINSTANCE.getMilestone_RequiredResults(),
					items, -1);
		}
	}
}