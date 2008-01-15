/**
 * <copyright>
 * </copyright>
 *
 * $Id: TaskDescriptorImpl.java,v 1.1 2008/01/15 08:51:36 jtham Exp $
 */
package org.eclipse.epf.xml.uma.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.BasicFeatureMap;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.xml.uma.Section;
import org.eclipse.epf.xml.uma.TaskDescriptor;
import org.eclipse.epf.xml.uma.UmaPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Task Descriptor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getTask <em>Task</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getPerformedPrimarilyBy <em>Performed Primarily By</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getGroup2 <em>Group2</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getAdditionallyPerformedBy <em>Additionally Performed By</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getAssistedBy <em>Assisted By</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getExternalInput <em>External Input</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getMandatoryInput <em>Mandatory Input</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getOptionalInput <em>Optional Input</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getOutput <em>Output</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#getStep <em>Step</em>}</li>
 *   <li>{@link org.eclipse.epf.xml.uma.impl.TaskDescriptorImpl#isIsSynchronizedWithSource <em>Is Synchronized With Source</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TaskDescriptorImpl extends WorkBreakdownElementImpl implements TaskDescriptor {
	/**
	 * The default value of the '{@link #getTask() <em>Task</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTask()
	 * @generated
	 * @ordered
	 */
	protected static final String TASK_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTask() <em>Task</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTask()
	 * @generated
	 * @ordered
	 */
	protected String task = TASK_EDEFAULT;

	/**
	 * The default value of the '{@link #getPerformedPrimarilyBy() <em>Performed Primarily By</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPerformedPrimarilyBy()
	 * @generated
	 * @ordered
	 */
	protected static final String PERFORMED_PRIMARILY_BY_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPerformedPrimarilyBy() <em>Performed Primarily By</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPerformedPrimarilyBy()
	 * @generated
	 * @ordered
	 */
	protected String performedPrimarilyBy = PERFORMED_PRIMARILY_BY_EDEFAULT;

	/**
	 * The cached value of the '{@link #getGroup2() <em>Group2</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getGroup2()
	 * @generated
	 * @ordered
	 */
	protected FeatureMap group2;

	/**
	 * The cached value of the '{@link #getStep() <em>Step</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStep()
	 * @generated
	 * @ordered
	 */
	protected EList step;

	/**
	 * The default value of the '{@link #isIsSynchronizedWithSource() <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsSynchronizedWithSource()
	 * @generated
	 * @ordered
	 */
	protected static final boolean IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isIsSynchronizedWithSource() <em>Is Synchronized With Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsSynchronizedWithSource()
	 * @generated
	 * @ordered
	 */
	protected boolean isSynchronizedWithSource = IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT;

	/**
	 * This is true if the Is Synchronized With Source attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isSynchronizedWithSourceESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TaskDescriptorImpl() {
		super();
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
	public String getTask() {
		return task;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTask(String newTask) {
		String oldTask = task;
		task = newTask;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.TASK_DESCRIPTOR__TASK, oldTask, task));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPerformedPrimarilyBy() {
		return performedPrimarilyBy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPerformedPrimarilyBy(String newPerformedPrimarilyBy) {
		String oldPerformedPrimarilyBy = performedPrimarilyBy;
		performedPrimarilyBy = newPerformedPrimarilyBy;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY, oldPerformedPrimarilyBy, performedPrimarilyBy));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FeatureMap getGroup2() {
		if (group2 == null) {
			group2 = new BasicFeatureMap(this, UmaPackage.TASK_DESCRIPTOR__GROUP2);
		}
		return group2;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getAdditionallyPerformedBy() {
		return getGroup2().list(UmaPackage.Literals.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getAssistedBy() {
		return getGroup2().list(UmaPackage.Literals.TASK_DESCRIPTOR__ASSISTED_BY);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getExternalInput() {
		return getGroup2().list(UmaPackage.Literals.TASK_DESCRIPTOR__EXTERNAL_INPUT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getMandatoryInput() {
		return getGroup2().list(UmaPackage.Literals.TASK_DESCRIPTOR__MANDATORY_INPUT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getOptionalInput() {
		return getGroup2().list(UmaPackage.Literals.TASK_DESCRIPTOR__OPTIONAL_INPUT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getOutput() {
		return getGroup2().list(UmaPackage.Literals.TASK_DESCRIPTOR__OUTPUT);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getStep() {
		if (step == null) {
			step = new EObjectContainmentEList(Section.class, this, UmaPackage.TASK_DESCRIPTOR__STEP);
		}
		return step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isIsSynchronizedWithSource() {
		return isSynchronizedWithSource;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIsSynchronizedWithSource(boolean newIsSynchronizedWithSource) {
		boolean oldIsSynchronizedWithSource = isSynchronizedWithSource;
		isSynchronizedWithSource = newIsSynchronizedWithSource;
		boolean oldIsSynchronizedWithSourceESet = isSynchronizedWithSourceESet;
		isSynchronizedWithSourceESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE, oldIsSynchronizedWithSource, isSynchronizedWithSource, !oldIsSynchronizedWithSourceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIsSynchronizedWithSource() {
		boolean oldIsSynchronizedWithSource = isSynchronizedWithSource;
		boolean oldIsSynchronizedWithSourceESet = isSynchronizedWithSourceESet;
		isSynchronizedWithSource = IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT;
		isSynchronizedWithSourceESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE, oldIsSynchronizedWithSource, IS_SYNCHRONIZED_WITH_SOURCE_EDEFAULT, oldIsSynchronizedWithSourceESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIsSynchronizedWithSource() {
		return isSynchronizedWithSourceESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case UmaPackage.TASK_DESCRIPTOR__GROUP2:
				return ((InternalEList)getGroup2()).basicRemove(otherEnd, msgs);
			case UmaPackage.TASK_DESCRIPTOR__STEP:
				return ((InternalEList)getStep()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmaPackage.TASK_DESCRIPTOR__TASK:
				return getTask();
			case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
				return getPerformedPrimarilyBy();
			case UmaPackage.TASK_DESCRIPTOR__GROUP2:
				if (coreType) return getGroup2();
				return ((FeatureMap.Internal)getGroup2()).getWrapper();
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
			case UmaPackage.TASK_DESCRIPTOR__STEP:
				return getStep();
			case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
				return isIsSynchronizedWithSource() ? Boolean.TRUE : Boolean.FALSE;
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
			case UmaPackage.TASK_DESCRIPTOR__TASK:
				setTask((String)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
				setPerformedPrimarilyBy((String)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__GROUP2:
				((FeatureMap.Internal)getGroup2()).set(newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY:
				getAdditionallyPerformedBy().clear();
				getAdditionallyPerformedBy().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY:
				getAssistedBy().clear();
				getAssistedBy().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT:
				getExternalInput().clear();
				getExternalInput().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT:
				getMandatoryInput().clear();
				getMandatoryInput().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT:
				getOptionalInput().clear();
				getOptionalInput().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__OUTPUT:
				getOutput().clear();
				getOutput().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__STEP:
				getStep().clear();
				getStep().addAll((Collection)newValue);
				return;
			case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
				setIsSynchronizedWithSource(((Boolean)newValue).booleanValue());
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
			case UmaPackage.TASK_DESCRIPTOR__TASK:
				setTask(TASK_EDEFAULT);
				return;
			case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
				setPerformedPrimarilyBy(PERFORMED_PRIMARILY_BY_EDEFAULT);
				return;
			case UmaPackage.TASK_DESCRIPTOR__GROUP2:
				getGroup2().clear();
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
			case UmaPackage.TASK_DESCRIPTOR__STEP:
				getStep().clear();
				return;
			case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
				unsetIsSynchronizedWithSource();
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
		switch (featureID) {
			case UmaPackage.TASK_DESCRIPTOR__TASK:
				return TASK_EDEFAULT == null ? task != null : !TASK_EDEFAULT.equals(task);
			case UmaPackage.TASK_DESCRIPTOR__PERFORMED_PRIMARILY_BY:
				return PERFORMED_PRIMARILY_BY_EDEFAULT == null ? performedPrimarilyBy != null : !PERFORMED_PRIMARILY_BY_EDEFAULT.equals(performedPrimarilyBy);
			case UmaPackage.TASK_DESCRIPTOR__GROUP2:
				return group2 != null && !group2.isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__ADDITIONALLY_PERFORMED_BY:
				return !getAdditionallyPerformedBy().isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__ASSISTED_BY:
				return !getAssistedBy().isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__EXTERNAL_INPUT:
				return !getExternalInput().isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__MANDATORY_INPUT:
				return !getMandatoryInput().isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__OPTIONAL_INPUT:
				return !getOptionalInput().isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__OUTPUT:
				return !getOutput().isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__STEP:
				return step != null && !step.isEmpty();
			case UmaPackage.TASK_DESCRIPTOR__IS_SYNCHRONIZED_WITH_SOURCE:
				return isSetIsSynchronizedWithSource();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (task: ");
		result.append(task);
		result.append(", performedPrimarilyBy: ");
		result.append(performedPrimarilyBy);
		result.append(", group2: ");
		result.append(group2);
		result.append(", isSynchronizedWithSource: ");
		if (isSynchronizedWithSourceESet) result.append(isSynchronizedWithSource); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //TaskDescriptorImpl