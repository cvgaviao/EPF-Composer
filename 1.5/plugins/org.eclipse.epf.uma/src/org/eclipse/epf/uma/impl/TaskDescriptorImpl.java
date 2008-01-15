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
package org.eclipse.epf.uma.impl;

import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.RoleDescriptor;
import org.eclipse.epf.uma.Section;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProductDescriptor;
import org.eclipse.epf.uma.ecore.util.DefaultValueManager;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Task Descriptor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getIsSynchronizedWithSource <em>Is Synchronized With Source</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getTask <em>Task</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getAdditionallyPerformedBy <em>Additionally Performed By</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getAssistedBy <em>Assisted By</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getExternalInput <em>External Input</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getMandatoryInput <em>Mandatory Input</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getOptionalInput <em>Optional Input</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getOutput <em>Output</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getPerformedPrimarilyBy <em>Performed Primarily By</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.TaskDescriptorImpl#getSelectedSteps <em>Selected Steps</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TaskDescriptorImpl extends WorkBreakdownElementImpl implements
		TaskDescriptor {
	/**
	 * Comment for <code>serialVersionUID</code>
	 */
	private static final long serialVersionUID = -4419414664273910863L;

	/**
	 * The default value of the '{@link #getIsSynchronizedWithSource() <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsSynchronizedWithSource()
	 * @generated
	 * @ordered
	 */
	protected static final Boolean IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT = Boolean.TRUE;

	/**
	 * The cached value of the '{@link #getIsSynchronizedWithSource() <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIsSynchronizedWithSource()
	 * @generated
	 * @ordered
	 */
	protected Boolean isSynchronizedWithSource = IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getTask() <em>Task</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTask()
	 * @generated
	 * @ordered
	 */
	protected Task task = null;

	/**
	 * The cached value of the '{@link #getAdditionallyPerformedBy() <em>Additionally Performed By</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdditionallyPerformedBy()
	 * @generated
	 * @ordered
	 */
	protected EList additionallyPerformedBy = null;

	/**
	 * The cached value of the '{@link #getAssistedBy() <em>Assisted By</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAssistedBy()
	 * @generated
	 * @ordered
	 */
	protected EList assistedBy = null;

	/**
	 * The cached value of the '{@link #getExternalInput() <em>External Input</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExternalInput()
	 * @generated
	 * @ordered
	 */
	protected EList externalInput = null;

	/**
	 * The cached value of the '{@link #getMandatoryInput() <em>Mandatory Input</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMandatoryInput()
	 * @generated
	 * @ordered
	 */
	protected EList mandatoryInput = null;

	/**
	 * The cached value of the '{@link #getOptionalInput() <em>Optional Input</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOptionalInput()
	 * @generated
	 * @ordered
	 */
	protected EList optionalInput = null;

	/**
	 * The cached value of the '{@link #getOutput() <em>Output</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOutput()
	 * @generated
	 * @ordered
	 */
	protected EList output = null;

	/**
	 * The cached value of the '{@link #getPerformedPrimarilyBy() <em>Performed Primarily By</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPerformedPrimarilyBy()
	 * @generated
	 * @ordered
	 */
	protected RoleDescriptor performedPrimarilyBy = null;

	/**
	 * The cached value of the '{@link #getSelectedSteps() <em>Selected Steps</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSelectedSteps()
	 * @generated
	 * @ordered
	 */
	protected EList selectedSteps = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TaskDescriptorImpl() {
		super();

		//UMA-->
		reassignDefaultValues();
		//UMA<--
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return UmaPackage.Literals.TASK_DESCRIPTOR;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Boolean getIsSynchronizedWithSource() {
		return isSynchronizedWithSource;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsSynchronizedWithSource(Boolean newIsSynchronizedWithSource) {
		Boolean oldIsSynchronizedWithSource = isSynchronizedWithSource;
		isSynchronizedWithSource = newIsSynchronizedWithSource;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE,
					oldIsSynchronizedWithSource, isSynchronizedWithSource));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Task getTask() {
		if (task != null && ((EObject) task).eIsProxy()) {
			InternalEObject oldTask = (InternalEObject) task;
			task = (Task) eResolveProxy(oldTask);
			if (task != oldTask) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.TASK_DESCRIPTOR__TASK, oldTask, task));
			}
		}
		return task;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Task basicGetTask() {
		return task;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTask(Task newTask) {
		Task oldTask = task;
		task = newTask;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.TASK_DESCRIPTOR__TASK, oldTask, task));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getAdditionallyPerformedBy() {
		if (additionallyPerformedBy == null) {
			additionallyPerformedBy = new EObjectResolvingEList(
					RoleDescriptor.class, this,
					UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY);
		}
		return additionallyPerformedBy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getAssistedBy() {
		if (assistedBy == null) {
			assistedBy = new EObjectResolvingEList(RoleDescriptor.class, this,
					UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY);
		}
		return assistedBy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getExternalInput() {
		if (externalInput == null) {
			externalInput = new EObjectResolvingEList(
					WorkProductDescriptor.class, this,
					UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT);
		}
		return externalInput;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getMandatoryInput() {
		if (mandatoryInput == null) {
			mandatoryInput = new EObjectResolvingEList(
					WorkProductDescriptor.class, this,
					UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT);
		}
		return mandatoryInput;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getOptionalInput() {
		if (optionalInput == null) {
			optionalInput = new EObjectResolvingEList(
					WorkProductDescriptor.class, this,
					UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT);
		}
		return optionalInput;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getOutput() {
		if (output == null) {
			output = new EObjectResolvingEList(WorkProductDescriptor.class,
					this, UmaPackage.TASK_DESCRIPTOR__OUTPUT);
		}
		return output;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RoleDescriptor getPerformedPrimarilyBy() {
		if (performedPrimarilyBy != null
				&& ((EObject) performedPrimarilyBy).eIsProxy()) {
			InternalEObject oldPerformedPrimarilyBy = (InternalEObject) performedPrimarilyBy;
			performedPrimarilyBy = (RoleDescriptor) eResolveProxy(oldPerformedPrimarilyBy);
			if (performedPrimarilyBy != oldPerformedPrimarilyBy) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
							UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY,
							oldPerformedPrimarilyBy, performedPrimarilyBy));
			}
		}
		return performedPrimarilyBy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RoleDescriptor basicGetPerformedPrimarilyBy() {
		return performedPrimarilyBy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPerformedPrimarilyBy(RoleDescriptor newPerformedPrimarilyBy) {
		RoleDescriptor oldPerformedPrimarilyBy = performedPrimarilyBy;
		performedPrimarilyBy = newPerformedPrimarilyBy;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY,
					oldPerformedPrimarilyBy, performedPrimarilyBy));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getSelectedSteps() {
		if (selectedSteps == null) {
			selectedSteps = new EObjectResolvingEList(Section.class, this,
					UmaPackage.TASK_DESCRIPTOR__SELECTED_STEPS);
		}
		return selectedSteps;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			return getIsSynchronizedWithSource();
		case UmaPackage.TASK_DESCRIPTOR__TASK:
			if (resolve)
				return getTask();
			return basicGetTask();
		case UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY:
			return getAdditionallyPerformedBy();
		case UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY:
			return getAssistedBy();
		case UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT:
			return getExternalInput();
		case UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT:
			return getMandatoryInput();
		case UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT:
			return getOptionalInput();
		case UmaPackage.TASK_DESCRIPTOR__OUTPUT:
			return getOutput();
		case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
			if (resolve)
				return getPerformedPrimarilyBy();
			return basicGetPerformedPrimarilyBy();
		case UmaPackage.TASK_DESCRIPTOR__SELECTED_STEPS:
			return getSelectedSteps();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			setIsSynchronizedWithSource((Boolean) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__TASK:
			setTask((Task) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY:
			getAdditionallyPerformedBy().clear();
			getAdditionallyPerformedBy().addAll((Collection) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY:
			getAssistedBy().clear();
			getAssistedBy().addAll((Collection) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT:
			getExternalInput().clear();
			getExternalInput().addAll((Collection) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT:
			getMandatoryInput().clear();
			getMandatoryInput().addAll((Collection) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT:
			getOptionalInput().clear();
			getOptionalInput().addAll((Collection) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__OUTPUT:
			getOutput().clear();
			getOutput().addAll((Collection) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
			setPerformedPrimarilyBy((RoleDescriptor) newValue);
			return;
		case UmaPackage.TASK_DESCRIPTOR__SELECTED_STEPS:
			getSelectedSteps().clear();
			getSelectedSteps().addAll((Collection) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(int featureID) {
		switch (featureID) {
		case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			setIsSynchronizedWithSource(IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT);
			return;
		case UmaPackage.TASK_DESCRIPTOR__TASK:
			setTask((Task) null);
			return;
		case UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY:
			getAdditionallyPerformedBy().clear();
			return;
		case UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY:
			getAssistedBy().clear();
			return;
		case UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT:
			getExternalInput().clear();
			return;
		case UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT:
			getMandatoryInput().clear();
			return;
		case UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT:
			getOptionalInput().clear();
			return;
		case UmaPackage.TASK_DESCRIPTOR__OUTPUT:
			getOutput().clear();
			return;
		case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
			setPerformedPrimarilyBy((RoleDescriptor) null);
			return;
		case UmaPackage.TASK_DESCRIPTOR__SELECTED_STEPS:
			getSelectedSteps().clear();
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(int featureID) {
		//UMA-->
		EStructuralFeature feature = getFeatureWithOverridenDefaultValue(featureID);
		if (feature != null) {
			return isFeatureWithOverridenDefaultValueSet(feature);
		}
		//UMA<--		
		switch (featureID) {
		case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
			return IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT == null ? isSynchronizedWithSource != null
					: !IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT
							.equals(isSynchronizedWithSource);
		case UmaPackage.TASK_DESCRIPTOR__TASK:
			return task != null;
		case UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY:
			return additionallyPerformedBy != null
					&& !additionallyPerformedBy.isEmpty();
		case UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY:
			return assistedBy != null && !assistedBy.isEmpty();
		case UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT:
			return externalInput != null && !externalInput.isEmpty();
		case UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT:
			return mandatoryInput != null && !mandatoryInput.isEmpty();
		case UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT:
			return optionalInput != null && !optionalInput.isEmpty();
		case UmaPackage.TASK_DESCRIPTOR__OUTPUT:
			return output != null && !output.isEmpty();
		case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
			return performedPrimarilyBy != null;
		case UmaPackage.TASK_DESCRIPTOR__SELECTED_STEPS:
			return selectedSteps != null && !selectedSteps.isEmpty();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int eBaseStructuralFeatureID(int derivedFeatureID, Class baseClass) {
		if (baseClass == Descriptor.class) {
			switch (derivedFeatureID) {
			case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
				return UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE;
			default:
				return -1;
			}
		}
		return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int eDerivedStructuralFeatureID(int baseFeatureID, Class baseClass) {
		if (baseClass == Descriptor.class) {
			switch (baseFeatureID) {
			case UmaPackage.DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
				return UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE;
			default:
				return -1;
			}
		}
		return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (isSynchronizedWithSource: "); //$NON-NLS-1$
		result.append(isSynchronizedWithSource);
		result.append(')');
		return result.toString();
	}

	////////////////////////
	// Begin of custom code 
	////////////////////////

	static {
		// override the default value of BreakdownElement.isPlanned
		//
		DefaultValueManager.INSTANCE.setStaticDefaultValue(UmaPackage.eINSTANCE
				.getTaskDescriptor(), UmaPackage.eINSTANCE
				.getBreakdownElement_IsPlanned(), Boolean.FALSE);
	}
} //TaskDescriptorImpl
