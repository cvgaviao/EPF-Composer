/**
 * <copyright>
 * </copyright>
 *
 * $Id: RegionImpl.java,v 1.1 2008/01/15 08:52:01 jtham Exp $
 */
package org.eclipse.epf.uma.impl;

import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.epf.uma.Region;
import org.eclipse.epf.uma.State;
import org.eclipse.epf.uma.StateMachine;
import org.eclipse.epf.uma.Transition;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.Vertex;
import org.eclipse.epf.uma.ecore.impl.MultiResourceEObject;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Region</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.epf.uma.impl.RegionImpl#getVertex <em>Vertex</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.RegionImpl#getTransition <em>Transition</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.RegionImpl#getState <em>State</em>}</li>
 *   <li>{@link org.eclipse.epf.uma.impl.RegionImpl#getStateMachine <em>State Machine</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RegionImpl extends MultiResourceEObject implements Region {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The cached value of the '{@link #getVertex() <em>Vertex</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVertex()
	 * @generated
	 * @ordered
	 */
	protected EList vertex = null;

	/**
	 * The cached value of the '{@link #getTransition() <em>Transition</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransition()
	 * @generated
	 * @ordered
	 */
	protected EList transition = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RegionImpl() {
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
		return UmaPackage.Literals.REGION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getVertex() {
		if (vertex == null) {
			vertex = new EObjectContainmentWithInverseEList.Resolving(
					Vertex.class, this, UmaPackage.REGION__VERTEX,
					UmaPackage.VERTEX__CONTAINER);
		}
		return vertex;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public List getTransition() {
		if (transition == null) {
			transition = new EObjectContainmentWithInverseEList.Resolving(
					Transition.class, this, UmaPackage.REGION__TRANSITION,
					UmaPackage.TRANSITION__CONTAINER);
		}
		return transition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public State getState() {
		if (eContainerFeatureID != UmaPackage.REGION__STATE)
			return null;
		return (State) eContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public State basicGetState() {
		if (eContainerFeatureID != UmaPackage.REGION__STATE)
			return null;
		return (State) eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetState(State newState,
			NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newState,
				UmaPackage.REGION__STATE, msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setState(State newState) {
		if (newState != eInternalContainer()
				|| (eContainerFeatureID != UmaPackage.REGION__STATE && newState != null)) {
			if (EcoreUtil.isAncestor(this, (EObject) newState))
				throw new IllegalArgumentException(
						"Recursive containment not allowed for " + toString()); //$NON-NLS-1$
			NotificationChain msgs = null;
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			if (newState != null)
				msgs = ((InternalEObject) newState).eInverseAdd(this,
						UmaPackage.STATE__REGION, State.class, msgs);
			msgs = basicSetState(newState, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.REGION__STATE, newState, newState));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public StateMachine getStateMachine() {
		if (eContainerFeatureID != UmaPackage.REGION__STATE_MACHINE)
			return null;
		return (StateMachine) eContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public StateMachine basicGetStateMachine() {
		if (eContainerFeatureID != UmaPackage.REGION__STATE_MACHINE)
			return null;
		return (StateMachine) eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetStateMachine(StateMachine newStateMachine,
			NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newStateMachine,
				UmaPackage.REGION__STATE_MACHINE, msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStateMachine(StateMachine newStateMachine) {
		if (newStateMachine != eInternalContainer()
				|| (eContainerFeatureID != UmaPackage.REGION__STATE_MACHINE && newStateMachine != null)) {
			if (EcoreUtil.isAncestor(this, (EObject) newStateMachine))
				throw new IllegalArgumentException(
						"Recursive containment not allowed for " + toString()); //$NON-NLS-1$
			NotificationChain msgs = null;
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			if (newStateMachine != null)
				msgs = ((InternalEObject) newStateMachine).eInverseAdd(this,
						UmaPackage.STATE_MACHINE__REGION, StateMachine.class,
						msgs);
			msgs = basicSetStateMachine(newStateMachine, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					UmaPackage.REGION__STATE_MACHINE, newStateMachine,
					newStateMachine));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseAdd(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case UmaPackage.REGION__VERTEX:
			return ((InternalEList) getVertex()).basicAdd(otherEnd, msgs);
		case UmaPackage.REGION__TRANSITION:
			return ((InternalEList) getTransition()).basicAdd(otherEnd, msgs);
		case UmaPackage.REGION__STATE:
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			return basicSetState((State) otherEnd, msgs);
		case UmaPackage.REGION__STATE_MACHINE:
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			return basicSetStateMachine((StateMachine) otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case UmaPackage.REGION__VERTEX:
			return ((InternalEList) getVertex()).basicRemove(otherEnd, msgs);
		case UmaPackage.REGION__TRANSITION:
			return ((InternalEList) getTransition())
					.basicRemove(otherEnd, msgs);
		case UmaPackage.REGION__STATE:
			return basicSetState(null, msgs);
		case UmaPackage.REGION__STATE_MACHINE:
			return basicSetStateMachine(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eBasicRemoveFromContainerFeature(
			NotificationChain msgs) {
		switch (eContainerFeatureID) {
		case UmaPackage.REGION__STATE:
			return eInternalContainer().eInverseRemove(this,
					UmaPackage.STATE__REGION, State.class, msgs);
		case UmaPackage.REGION__STATE_MACHINE:
			return eInternalContainer().eInverseRemove(this,
					UmaPackage.STATE_MACHINE__REGION, StateMachine.class, msgs);
		}
		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case UmaPackage.REGION__VERTEX:
			return getVertex();
		case UmaPackage.REGION__TRANSITION:
			return getTransition();
		case UmaPackage.REGION__STATE:
			if (resolve)
				return getState();
			return basicGetState();
		case UmaPackage.REGION__STATE_MACHINE:
			if (resolve)
				return getStateMachine();
			return basicGetStateMachine();
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
		case UmaPackage.REGION__VERTEX:
			getVertex().clear();
			getVertex().addAll((Collection) newValue);
			return;
		case UmaPackage.REGION__TRANSITION:
			getTransition().clear();
			getTransition().addAll((Collection) newValue);
			return;
		case UmaPackage.REGION__STATE:
			setState((State) newValue);
			return;
		case UmaPackage.REGION__STATE_MACHINE:
			setStateMachine((StateMachine) newValue);
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
		case UmaPackage.REGION__VERTEX:
			getVertex().clear();
			return;
		case UmaPackage.REGION__TRANSITION:
			getTransition().clear();
			return;
		case UmaPackage.REGION__STATE:
			setState((State) null);
			return;
		case UmaPackage.REGION__STATE_MACHINE:
			setStateMachine((StateMachine) null);
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
		case UmaPackage.REGION__VERTEX:
			return vertex != null && !vertex.isEmpty();
		case UmaPackage.REGION__TRANSITION:
			return transition != null && !transition.isEmpty();
		case UmaPackage.REGION__STATE:
			return basicGetState() != null;
		case UmaPackage.REGION__STATE_MACHINE:
			return basicGetStateMachine() != null;
		}
		return super.eIsSet(featureID);
	}

} //RegionImpl