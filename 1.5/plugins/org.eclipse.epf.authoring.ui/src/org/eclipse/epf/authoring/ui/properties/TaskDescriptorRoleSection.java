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
import org.eclipse.epf.authoring.ui.filters.ProcessRoleFilter;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.itemsfilter.FilterConstants;
import org.eclipse.epf.library.edit.process.command.AssignPrimaryPerformerToTaskDescriptor;
import org.eclipse.epf.library.edit.process.command.AssignRoleToTaskDescriptor;
import org.eclipse.epf.library.edit.process.command.IActionTypeConstants;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;


/**
 * The Task descriptor - role tab section
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 * 
 */
public class TaskDescriptorRoleSection extends RelationSection {
	private IFilter roleFilter = null;

	/**
	 * Get process roledescriptor filter
	 */
	public IFilter getFilter() {
		if (roleFilter == null) {
			roleFilter = new ProcessRoleFilter(getConfiguration(), null,
					FilterConstants.ROLES);
		}
		return roleFilter;
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
				List list = new ArrayList();
				RoleDescriptor role = ((TaskDescriptor) element)
						.getPerformedPrimarilyBy();
				if (role != null)
					list.add(role);
				return getFilteredList(list).toArray();
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
						((TaskDescriptor) element).getAdditionallyPerformedBy())
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
						((TaskDescriptor) element).getAssistedBy()).toArray();
			}
		};
		tableViewer3.setContentProvider(contentProvider);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#init()
	 */
	protected void init() {
		super.init();

		labelProvider = new AdapterFactoryLabelProvider(
				TngAdapterFactory.INSTANCE.getOBS_ComposedAdapterFactory());

		int numOfTables = 3;
		
		setTabData(PropertiesResources.TaskDescriptor_Roles_SectionTitle,
				PropertiesResources.TaskDescriptor_Roles_SectionDescription,
				PropertiesResources.TaskDescriptor_Roles_Table1,
				PropertiesResources.TaskDescriptor_Roles_Table2,
				PropertiesResources.TaskDescriptor_Roles_Table3,
				null,
				FilterConstants.ROLES);
		
		boolean[] changesAllowed = { true, true, true };
		setTableData(numOfTables, changesAllowed);

	}


	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#refresh()
	 */
	public void refresh() {
		// System.out.println("Refreshing TaskDescriptor ROLES Section");
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
				RoleDescriptor.class, items);

	}

	private List getRoles(List items) {
		List roleList = new ArrayList();
		if (!items.isEmpty()) {
			for (int i = 0; i < items.size(); i++) {
				roleList.add(((RoleDescriptor) items.get(i)).getRole());
			}
		}

		return roleList;
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems1(java.util.List)
	 */
	protected void addItems1(List items) {
		if (!items.isEmpty()) {
			List elementList = getRoles(((TaskDescriptor) element)
					.getAdditionallyPerformedBy());
			elementList.addAll(getRoles(((TaskDescriptor) element)
					.getAssistedBy()));

			List newList = new ArrayList();
			for (int i = 0; i < items.size(); i++) {
				Object obj = items.get(i);
				if (obj instanceof Role) {
					Role role = (Role) obj;
					if (!elementList.contains(role))
						newList.add(role);
				}
			}
			if (newList.size() > 0) {
				AssignPrimaryPerformerToTaskDescriptor cmd = new AssignPrimaryPerformerToTaskDescriptor(
						(TaskDescriptor) element, (Role) newList.get(0), getConfiguration());
				actionMgr.execute(cmd);
			}
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems2(java.util.List)
	 */
	protected void addItems2(List items) {
		if (!items.isEmpty()) {
			RoleDescriptor roleDesc = ((TaskDescriptor) element)
					.getPerformedPrimarilyBy();
			List elementList = getRoles(((TaskDescriptor) element)
					.getAssistedBy());
			if (roleDesc != null)
				elementList.add(roleDesc.getRole());

			List newList = new ArrayList();
			for (int i = 0; i < items.size(); i++) {
				Object obj = items.get(i);
				if (obj instanceof Role) {
					Role role = (Role) obj;
					if (!elementList.contains(role))
						newList.add(role);
				}
			}
			if (newList.size() > 0) {
				AssignRoleToTaskDescriptor cmd = new AssignRoleToTaskDescriptor(
						(TaskDescriptor) element, newList,
						IActionTypeConstants.ADD_ADDITIONAL_PERFORMER, getConfiguration());
				actionMgr.execute(cmd);
			}
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addItems3(java.util.List)
	 */
	protected void addItems3(List items) {
		if (!items.isEmpty()) {
			RoleDescriptor roleDesc = ((TaskDescriptor) element)
					.getPerformedPrimarilyBy();
			List elementList = getRoles(((TaskDescriptor) element)
					.getAdditionallyPerformedBy());
			if (roleDesc != null)
				elementList.add(roleDesc.getRole());

			List newList = new ArrayList();
			for (int i = 0; i < items.size(); i++) {
				Object obj = items.get(i);
				if (obj instanceof Role) {
					Role role = (Role) obj;
					if (!elementList.contains(role))
						newList.add(role);
				}
			}
			if (newList.size() > 0) {
				AssignRoleToTaskDescriptor cmd = new AssignRoleToTaskDescriptor(
						(TaskDescriptor) element, newList,
						IActionTypeConstants.ADD_ASSISTED_BY, getConfiguration());
				actionMgr.execute(cmd);
			}
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems1(java.util.List)
	 */
	protected void addFromProcessItems1(List items) {
		if (!items.isEmpty()) {
			items.removeAll(((TaskDescriptor) element)
					.getAdditionallyPerformedBy());
			items.removeAll(((TaskDescriptor) element).getAssistedBy());

			if (items.size() > 0) {
				actionMgr.doAction(IActionManager.SET, element,
						UmaPackage.eINSTANCE
								.getTaskDescriptor_PerformedPrimarilyBy(),
						items.get(0), -1);
			}
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems2(java.util.List)
	 */
	protected void addFromProcessItems2(List items) {
		if (!items.isEmpty()) {
			items.removeAll(((TaskDescriptor) element).getAssistedBy());
			items.remove(((TaskDescriptor) element).getPerformedPrimarilyBy());

			actionMgr.doAction(IActionManager.ADD_MANY, element,
					UmaPackage.eINSTANCE
							.getTaskDescriptor_AdditionallyPerformedBy(),
					items, -1);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#addFromProcessItems3(java.util.List)
	 */
	protected void addFromProcessItems3(List items) {
		if (!items.isEmpty()) {
			items.removeAll(((TaskDescriptor) element)
					.getAdditionallyPerformedBy());
			items.remove(((TaskDescriptor) element).getPerformedPrimarilyBy());

			actionMgr.doAction(IActionManager.ADD_MANY, element,
					UmaPackage.eINSTANCE.getTaskDescriptor_AssistedBy(), items,
					-1);
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems1(java.util.List)
	 */
	protected void removeItems1(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items, UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems2(java.util.List)
	 */
	protected void removeItems2(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items,
					UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#removeItems3(java.util.List)
	 */
	protected void removeItems3(List items) {
		if (!items.isEmpty()) {
			RemoveDescriptorCommand cmd = new RemoveDescriptorCommand(element,
					items, UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY);
			actionMgr.execute(cmd);
		}
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements1()
	 */
	protected List getExistingElements1() {
		List list = new ArrayList();
		RoleDescriptor roleDesc = ((TaskDescriptor) element)
				.getPerformedPrimarilyBy();
		if (roleDesc != null)
			list.add(roleDesc);

		return list;
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements2()
	 */
	protected List getExistingElements2() {
		return ((TaskDescriptor) element).getAdditionallyPerformedBy();
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getExistingElements3()
	 */
	protected List getExistingElements3() {
		return ((TaskDescriptor) element).getAssistedBy();
	};

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getFilterForDescriptors()
	 */
	protected IFilter getFilterForDescriptors() {
		return new DescriptorProcessFilter(getConfiguration()) {
			protected boolean childAccept(Object obj) {
				if (obj instanceof Activity) {
					List list = new ArrayList();
					getActivitiesInScope(TngAdapterFactory.INSTANCE
							.getWBS_ComposedAdapterFactory(), element, list);
					if (list.contains(obj))
						return true;
					else
						return false;
				}
				if (obj instanceof RoleDescriptor)
					return true;
				return false;
			}
		};
	}

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
	 * @see org.eclipse.epf.authoring.ui.properties.RelationSection#getDescriptorTabName()
	 */
	protected String getDescriptorTabName() {
		return FilterConstants.ROLE_DESCRIPTORS;
	}

}