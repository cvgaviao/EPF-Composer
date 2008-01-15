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
import org.eclipse.epf.library.edit.process.command.AssignWPToTaskDescriptor;
import org.eclipse.epf.library.edit.process.command.IActionTypeConstants;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;


/**
 * The task descriptor - work product section
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 * 
 */
public class TaskDescriptorWorkProductSection extends RelationSection {
	private IFilter filter = null;

	/**
	 * Get process work product filter
	 */
	public IFilter getFilter() {
		if (filter == null) {
			filter = new ProcessWorkProductFilter(getConfiguration(), null,
					FilterConstants.WORKPRODUCTS);
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
						((TaskDescriptor) element).getMandatoryInput())
						.toArray();
			}
		};
		tableViewer1.setContentProvider(contentProvider);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#initContentProvider2()
	 */
	protected void initContentProvider2() {
		contentProvider = new AdapterFactoryContentProvider(getAdapterFactory()) {
			public Object[] getElements(Object object) {
				return getFilteredList(
						((TaskDescriptor) element).getOptionalInput())
						.toArray();
			}
		};
		tableViewer2.setContentProvider(contentProvider);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#initContentProvider3()
	 */
	protected void initContentProvider3() {
		contentProvider = new AdapterFactoryContentProvider(getAdapterFactory()) {
			public Object[] getElements(Object object) {
				return getFilteredList(
						((TaskDescriptor) element).getExternalInput())
						.toArray();
			}
		};
		tableViewer3.setContentProvider(contentProvider);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#initContentProvider4()
	 */
	protected void initContentProvider4() {
		contentProvider = new AdapterFactoryContentProvider(getAdapterFactory()) {
			public Object[] getElements(Object object) {
				return getFilteredList(((TaskDescriptor) element).getOutput())
						.toArray();
			}
		};
		tableViewer4.setContentProvider(contentProvider);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#init()
	 */
	protected void init() {
		super.init();

		labelProvider = new AdapterFactoryLabelProvider(
				TngAdapterFactory.INSTANCE.getPBS_ComposedAdapterFactory());

		int numOfTables = 4;
		setTabData(PropertiesResources.TaskDescriptor_WorkProducts_SectionTitle,
				PropertiesResources.TaskDescriptor_WorkProducts_SectionDescription,
				PropertiesResources.TaskDescriptor_WorkProducts_Table1,
				PropertiesResources.TaskDescriptor_WorkProducts_Table2,
				PropertiesResources.TaskDescriptor_WorkProducts_Table3,
				PropertiesResources.TaskDescriptor_WorkProducts_Table4,
				FilterConstants.WORKPRODUCTS);
		
		boolean[] changesAllowed = { true, true, true, true };
		setTableData(numOfTables, changesAllowed);

	}


	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#refresh()
	 */
	public void refresh() {
		if (getElement() instanceof TaskDescriptor) {
			element = (TaskDescriptor) getElement();
		}
		super.refresh();
	}


	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getDescriptorsFromProcess()
	 */
	protected List getDescriptorsFromProcess() {
		List items = new ArrayList();
		return ProcessUtil.getElementsInScope(getAdapterFactory(), element,
				WorkProductDescriptor.class, items);
	}

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
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems1(java.util.List)
	 */
	protected void addItems1(List items) {
		if (!items.isEmpty()) {
			List elementList = getWorkProducts(((TaskDescriptor) element)
					.getOptionalInput());
			elementList.addAll(getWorkProducts(((TaskDescriptor) element)
					.getExternalInput()));
			List newList = new ArrayList();
			for (int i = 0; i < items.size(); i++) {
				Object obj = items.get(i);
				if (obj instanceof WorkProduct) {
					WorkProduct wp = (WorkProduct) obj;
					if (!elementList.contains(wp))
						newList.add(wp);
				}
			}
			if (newList.size() > 0) {
				AssignWPToTaskDescriptor cmd = new AssignWPToTaskDescriptor(
						(TaskDescriptor) element, newList,
						IActionTypeConstants.ADD_MANDATORY_INPUT,
						getConfiguration());
				actionMgr.execute(cmd);
			}
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems2(java.util.List)
	 */
	protected void addItems2(List items) {
		if (!items.isEmpty()) {
			List elementList = getWorkProducts(((TaskDescriptor) element)
					.getMandatoryInput());
			elementList.addAll(getWorkProducts(((TaskDescriptor) element)
					.getExternalInput()));

			List newList = new ArrayList();
			for (int i = 0; i < items.size(); i++) {
				Object obj = items.get(i);
				if (obj instanceof WorkProduct) {
					WorkProduct wp = (WorkProduct) obj;
					if (!elementList.contains(wp))
						newList.add(wp);
				}
			}

			if (newList.size() > 0) {
				AssignWPToTaskDescriptor cmd = new AssignWPToTaskDescriptor(
						(TaskDescriptor) element, newList,
						IActionTypeConstants.ADD_OPTIONAL_INPUT,
						getConfiguration());
				actionMgr.execute(cmd);
			}
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems3(java.util.List)
	 */
	protected void addItems3(List items) {
		if (!items.isEmpty()) {
			List elementList = getWorkProducts(((TaskDescriptor) element)
					.getMandatoryInput());
			elementList.addAll(getWorkProducts(((TaskDescriptor) element)
					.getOptionalInput()));

			List newList = new ArrayList();
			for (int i = 0; i < items.size(); i++) {
				Object obj = items.get(i);
				if (obj instanceof WorkProduct) {
					WorkProduct wp = (WorkProduct) obj;
					if (!elementList.contains(wp))
						newList.add(wp);
				}
			}
			if (newList.size() > 0) {
				AssignWPToTaskDescriptor cmd = new AssignWPToTaskDescriptor(
						(TaskDescriptor) element, newList,
						IActionTypeConstants.ADD_EXTERNAL_INPUT,
						getConfiguration());
				actionMgr.execute(cmd);
			}
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems4(java.util.List)
	 */
	protected void addItems4(List items) {
		if (!items.isEmpty()) {
			AssignWPToTaskDescriptor cmd = new AssignWPToTaskDescriptor(
					(TaskDescriptor) element, items,
					IActionTypeConstants.ADD_OUTPUT, getConfiguration());
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems1(java.util.List)
	 */
	protected void removeItems1(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items, UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems2(java.util.List)
	 */
	protected void removeItems2(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items, UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems3(java.util.List)
	 */
	protected void removeItems3(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items, UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems4(java.util.List)
	 */
	protected void removeItems4(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items, UmaPackage.TASK_DESCRIPTOR__OUTPUT);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements1()
	 */
	protected List getExistingElements1() {
		return ((TaskDescriptor) element).getMandatoryInput();
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements2()
	 */
	protected List getExistingElements2() {
		return ((TaskDescriptor) element).getOptionalInput();
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements3()
	 */
	protected List getExistingElements3() {
		return ((TaskDescriptor) element).getExternalInput();
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements4()
	 */
	protected List getExistingElements4() {
		return ((TaskDescriptor) element).getOutput();
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getProcess()
	 */
	protected Process getProcess() {
		AdapterFactory aFactory = TngAdapterFactory.INSTANCE
				.getWBS_ComposedAdapterFactory();
		ItemProviderAdapter adapter = (ItemProviderAdapter) aFactory.adapt(
				element, ITreeItemContentProvider.class);
		Object obj = ProcessUtil.getRootProcess(aFactory, adapter, element);
		return (Process) obj;
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getFilterForDescriptors()
	 */
	protected IFilter getFilterForDescriptors() {
		return new DescriptorProcessFilter(getConfiguration()) {
			protected boolean childAccept(Object obj) {
				if (obj instanceof Activity) {
					List list1 = new ArrayList();
					getActivitiesInScope(TngAdapterFactory.INSTANCE
							.getWBS_ComposedAdapterFactory(), element, list1);
					if (list1.contains(obj))
						return true;
					else
						return false;
				}
				if (obj instanceof WorkProductDescriptor)
					return true;
				return false;
			}
		};
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems1(java.util.List)
	 */
	protected void addFromProcessItems1(List items) {
		if (!items.isEmpty()) {
			items.removeAll(((TaskDescriptor) element).getOptionalInput());
			items.removeAll(((TaskDescriptor) element).getExternalInput());
			actionMgr.doAction(IActionManager.ADD_MANY, element,
					UmaPackage.eINSTANCE.getTaskDescriptor_MandatoryInput(),
					items, -1);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems2(java.util.List)
	 */
	protected void addFromProcessItems2(List items) {
		if (!items.isEmpty()) {
			items.removeAll(((TaskDescriptor) element).getMandatoryInput());
			items.removeAll(((TaskDescriptor) element).getExternalInput());
			actionMgr.doAction(IActionManager.ADD_MANY, element,
					UmaPackage.eINSTANCE.getTaskDescriptor_OptionalInput(),
					items, -1);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems3(java.util.List)
	 */
	protected void addFromProcessItems3(List items) {
		if (!items.isEmpty()) {
			items.removeAll(((TaskDescriptor) element).getMandatoryInput());
			items.removeAll(((TaskDescriptor) element).getOptionalInput());
			actionMgr.doAction(IActionManager.ADD_MANY, element,
					UmaPackage.eINSTANCE.getTaskDescriptor_ExternalInput(),
					items, -1);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems4(java.util.List)
	 */
	protected void addFromProcessItems4(List items) {
		if (!items.isEmpty()) {
			actionMgr.doAction(IActionManager.ADD_MANY, element,
					UmaPackage.eINSTANCE.getTaskDescriptor_Output(), items, -1);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getDescriptorTabName()
	 */
	protected String getDescriptorTabName() {
		return FilterConstants.WORK_PRODUCT_DESCRIPTORS;
	}

}