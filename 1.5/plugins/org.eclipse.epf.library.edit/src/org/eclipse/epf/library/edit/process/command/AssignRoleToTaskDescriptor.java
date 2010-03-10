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
package org.eclipse.epf.library.edit.process.command;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.ui.UserInteractionHelper;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.TeamProfile;
import org.eclipse.epf.uma.UmaPackage;


/**
 * Command for assign roles to task descriptor. It will assign primary, additional
 * performers and assisted by to a task descriptor
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 */
public class AssignRoleToTaskDescriptor extends AddMethodElementCommand {

	private List roles;

	private Activity activity;

	private TaskDescriptor taskDesc;

	private Collection modifiedResources;

	private Collection affectedObjects;
	
	private int action;

	List existingRoleDescList = new ArrayList();

	List newRoleDescList = new ArrayList();

	private HashMap map = new HashMap();

	private MethodConfiguration config;

	private boolean isNewRoleDescriptor = false;

	/**
	 * Assign role to task descriptor Used for both additionally performed by
	 * and assisted by
	 */
	public AssignRoleToTaskDescriptor(TaskDescriptor taskDesc, List roles,
			int action, MethodConfiguration config) {

		super(TngUtil.getOwningProcess(taskDesc));

		this.roles = roles;
		this.taskDesc = taskDesc;
		this.action = action;
		this.config = config;

		AdapterFactory aFactory = TngAdapterFactory.INSTANCE
				.getWBS_ComposedAdapterFactory();
		ItemProviderAdapter adapter = (ItemProviderAdapter) aFactory.adapt(
				taskDesc, ITreeItemContentProvider.class);
		Object parent = adapter.getParent(taskDesc);
		if (parent instanceof Activity) {
			this.activity = (Activity) parent;
		}

		this.modifiedResources = new HashSet();
		this.affectedObjects = new HashSet();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#execute()
	 */
	public void execute() {
		
		// add to default configuration if not there already
		if (!super.addToDefaultConfiguration(roles))
			return;

		for (Iterator it = roles.iterator(); it.hasNext();) {
			// boolean descExists = false;
			Role role = (Role) it.next();

			RoleDescriptor roleDesc = null;
			isNewRoleDescriptor = false;
			// check for local descriptor
			roleDesc = (RoleDescriptor) ProcessCommandUtil.getDescriptor(role,
					activity, config);
			if (roleDesc == null) {
				// check for inherited descriptor
				roleDesc = (RoleDescriptor) ProcessCommandUtil
						.getInheritedDescriptor(role, activity, config);
				if (roleDesc == null) {
					roleDesc = ProcessUtil.createRoleDescriptor(role);
					isNewRoleDescriptor = true;

				}
			}
			if (isNewRoleDescriptor)
				newRoleDescList.add(roleDesc);
			else
				existingRoleDescList.add(roleDesc);

			// get team
			TeamProfile team = UserInteractionHelper.getTeam(activity, role);
			if (team != null) {
				map.put(roleDesc, team);
			}
		}

		redo();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#redo()
	 */
	public void redo() {

		if (action == IActionTypeConstants.ADD_PRIMARY_PERFORMER) {
			taskDesc.getPerformedPrimarilyBy().addAll(existingRoleDescList);
			taskDesc.getPerformedPrimarilyBy().addAll(newRoleDescList);
		} else if (action == IActionTypeConstants.ADD_ADDITIONAL_PERFORMER) {
			taskDesc.getAdditionallyPerformedBy().addAll(existingRoleDescList);
			taskDesc.getAdditionallyPerformedBy().addAll(newRoleDescList);
		} else if (action == IActionTypeConstants.ADD_ASSISTED_BY) {
			taskDesc.getAssistedBy().addAll(existingRoleDescList);
			taskDesc.getAssistedBy().addAll(newRoleDescList);
		}
		addLocalUsingInfo(existingRoleDescList, getFeature(action));
		addLocalUsingInfo(newRoleDescList, getFeature(action));

		activity.getBreakdownElements().addAll(newRoleDescList);

		if (map != null) {
			Set keyset = map.keySet();
			for (Iterator itor = keyset.iterator(); itor.hasNext();) {
				Object key = itor.next();
				TeamProfile team = (TeamProfile) map.get(key);

				// add to team
				team.getTeamRoles().add((RoleDescriptor) key);
			}
		}

		
	}
	
	private EReference getFeature(int action) {
		UmaPackage up = UmaPackage.eINSTANCE;
		
		if (action == IActionTypeConstants.ADD_PRIMARY_PERFORMER) {
			return up.getTaskDescriptor_PerformedPrimarilyBy();		
		} 
		
		if (action == IActionTypeConstants.ADD_ADDITIONAL_PERFORMER) {
			return up.getTaskDescriptor_AdditionallyPerformedBy();	
		}

		if (action == IActionTypeConstants.ADD_ASSISTED_BY) {
			return up.getTaskDescriptor_AssistedBy();	
		}
		
		return null;
	}
	
	private void addLocalUsingInfo(List<Descriptor> deslIst, EReference feature) {
		if (! ProcessUtil.isSynFree() || deslIst == null || feature == null) {
			return;
		}		
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		for (Descriptor des : deslIst) {
			propUtil.addLocalUse(des, taskDesc, feature);
		}
		
	}
	
	private void removeLocalUsingInfo(List<Descriptor> deslIst, EReference feature) {
		if (! ProcessUtil.isSynFree() || deslIst == null || feature == null) {
			return;
		}		
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		for (Descriptor des : deslIst) {
			propUtil.removeLocalUse(des, taskDesc, feature);
		}
		
	}

	public void undo() {

		// basically remove from configuration if anything was added
		super.undo();

		if (action == IActionTypeConstants.ADD_ADDITIONAL_PERFORMER) {
			taskDesc.getPerformedPrimarilyBy().removeAll(
					existingRoleDescList);
			taskDesc.getPerformedPrimarilyBy().removeAll(newRoleDescList);
		} else	if (action == IActionTypeConstants.ADD_ADDITIONAL_PERFORMER) {
			taskDesc.getAdditionallyPerformedBy().removeAll(
					existingRoleDescList);
			taskDesc.getAdditionallyPerformedBy().removeAll(newRoleDescList);
		} else if (action == IActionTypeConstants.ADD_ASSISTED_BY) {
			taskDesc.getAssistedBy().removeAll(existingRoleDescList);
			taskDesc.getAssistedBy().removeAll(newRoleDescList);
		}
		
		removeLocalUsingInfo(existingRoleDescList, getFeature(action));
		removeLocalUsingInfo(newRoleDescList, getFeature(action));
		
		activity.getBreakdownElements().removeAll(newRoleDescList);

		if (map != null) {
			Set keyset = map.keySet();
			for (Iterator itor = keyset.iterator(); itor.hasNext();) {
				Object key = itor.next();
				TeamProfile team = (TeamProfile) map.get(key);

				// remove it from team
				team.getTeamRoles().remove((RoleDescriptor) key);
			}
		}
	}

	protected boolean prepare() {
		return true;
	}

	public Collection getModifiedResources() {

		if (roles != null && !roles.isEmpty()) {
			if (activity.eResource() != null) {
				modifiedResources.add(activity.eResource());
			}
			if (taskDesc.eResource() != null) {
				modifiedResources.add(taskDesc.eResource());
			}
		}
		return modifiedResources;
	}
	
	public Collection getAffectedObjects() {
		if (roles != null && !roles.isEmpty()) {
			affectedObjects.add(activity);
			affectedObjects.add(taskDesc);
			return affectedObjects;
		}
		return super.getAffectedObjects();
	}

}
