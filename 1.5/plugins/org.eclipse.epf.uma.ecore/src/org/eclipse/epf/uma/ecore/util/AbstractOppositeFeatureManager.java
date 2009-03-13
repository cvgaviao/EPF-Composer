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
package org.eclipse.epf.uma.ecore.util;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.CommonPlugin;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * 
 * @author Phong Nguyen Le
 * @since 1.5
 *
 */
public abstract class AbstractOppositeFeatureManager {
	private ArrayList<OppositeFeature> predefinedOppositeFeatures;

	protected AbstractOppositeFeatureManager() {
		registerPredefinedOppositeFeatures();
	}
	
	/**
	 * Registers the predefined opposite features.
	 */
	private void registerPredefinedOppositeFeatures() {
		predefinedOppositeFeatures = new ArrayList<OppositeFeature>();
		Field[] fields = getClass().getFields();
		for (int i = 0; i < fields.length; i++) {
			Field field = fields[i];
			int mod = field.getModifiers();
			if (Modifier.isPublic(mod) && Modifier.isStatic(mod)
					&& Modifier.isFinal(mod)
					&& field.getType() == OppositeFeature.class) {
				try {
					predefinedOppositeFeatures.add((OppositeFeature) field.get(this));
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				}
			}
		}

		for (OppositeFeature feature : predefinedOppositeFeatures) {
			OppositeFeature.registerOppositeFeature(feature);
		}
	}

	/**
	 * Gets the predefined opposite features.
	 * 
	 * @return the predefined opposite features
	 */
	public Collection<OppositeFeature> getPredefinedOppositeFeatures() {
		return predefinedOppositeFeatures;
	}

	/**
	 * Gets the value of an opposite feature.
	 * 
	 * @param feature
	 *            an opposite feature
	 * @return the value for the specified opposite feature
	 */
	public Object getOppositeFeatureValue(EObject eObject, OppositeFeature feature) {
		OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(eObject);
		if(adapter == null) {
			throw new IllegalArgumentException("Object is not managed by this manager.");
		}
		return adapter.getOppositeFeatureValue(feature);
	}

	/**
	 * Gets all the opposite features associated with this model object.
	 * 
	 * @return a collection of opposite features
	 */
	public Collection<OppositeFeature> getOppositeFeatures(EObject eObject) {
		OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(eObject);
		if(adapter == null) {
			throw new IllegalArgumentException("Object is not managed by this manager.");
		}
		return adapter.getOppositeFeatures();
	}
	
	public void manage(EObject eObject) {
		OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(eObject);
		if(adapter == null) {
			eObject.eAdapters().add(createOppositeFeatureAdapter(eObject));
		}
	}
	
	public void release(EObject eObject) {
		OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(eObject);
		if(adapter != null) {
			eObject.eAdapters().remove(adapter);
		}
	}
	
	private OppositeFeatureAdapter getOppositeFeatureAdapter(EObject eObject) {
		for (Adapter adapter : new ArrayList<Adapter>(eObject.eAdapters())) {
			if(adapter instanceof OppositeFeatureAdapter) {
				return (OppositeFeatureAdapter) adapter;
			}
		}
		return null;
	}
	
	private OppositeFeatureAdapter createOppositeFeatureAdapter(EObject eObject) {
		OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(eObject);
		if(adapter == null) {
			adapter = new OppositeFeatureAdapter();
			eObject.eAdapters().add(adapter);
		}
		return adapter;
	}

	private class OppositeFeatureAdapter extends AdapterImpl {
		private static final boolean DEBUG = false;
		
		/**
		 * A map of entries of OppositeFeature / OppositeFeature's value
		 */
		private Map<OppositeFeature, Object> oppositeFeatureMap;

		private boolean hasOppositeFeature = true;

		
		private OppositeFeatureAdapter() {
		}
		
		@Override
		public void notifyChanged(Notification msg) {
			if (msg.getEventType() == Notification.RESOLVE) {
				return;
			}
			
			Object f = msg.getFeature();
			if (f instanceof EStructuralFeature) {
				EStructuralFeature feature = (EStructuralFeature) f;
				OppositeFeature oppositeFeature = OppositeFeature
						.getOppositeFeature(feature);
				if (oppositeFeature != null) {
					EObject oldOtherEnd;
					EObject otherEnd;
					if (oppositeFeature.isMany()) {
						switch (msg.getEventType()) {
						case Notification.SET:
							oldOtherEnd = (EObject) msg.getOldValue();
							if (oppositeFeature.resolveOwner()) {
								oldOtherEnd = (EObject) resolve(oldOtherEnd);
							}
							if (oldOtherEnd != null) {
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(oldOtherEnd);
								adapter.oppositeRemove(oppositeFeature, msg
										.getNotifier());
							}
						case Notification.ADD:
							otherEnd = (EObject) msg.getNewValue();
							if (oppositeFeature.resolveOwner()) {
								otherEnd = (EObject) resolve(otherEnd);
								replace(feature, msg.getNewValue(), otherEnd);
							}
							if (otherEnd != null) {
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								adapter.oppositeAdd(oppositeFeature, msg
										.getNotifier());
							}
							break;
						case Notification.ADD_MANY:
							for (Iterator iter = ((Collection) msg.getNewValue())
									.iterator(); iter.hasNext();) {
								Object obj = iter.next();
								otherEnd = (EObject) obj;
								if (oppositeFeature.resolveOwner()) {
									otherEnd = (EObject) resolve(otherEnd);
									replace(feature, obj, otherEnd);
								}
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								adapter.oppositeAdd(oppositeFeature, msg
										.getNotifier());
							}
							break;
						case Notification.REMOVE:
							otherEnd = (EObject) msg.getOldValue();
							if (oppositeFeature.resolveOwner()) {
								otherEnd = (EObject) resolve(otherEnd);
							}
							if (otherEnd != null) {
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								adapter.oppositeRemove(oppositeFeature, msg
										.getNotifier());
							}
							break;
						case Notification.REMOVE_MANY:
							for (Iterator<?> iter = ((Collection) msg.getOldValue())
									.iterator(); iter.hasNext();) {
								otherEnd = (EObject) iter.next();
								if (oppositeFeature.resolveOwner()) {
									otherEnd = (EObject) resolve(otherEnd);
								}
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								adapter.oppositeRemove(oppositeFeature, msg
										.getNotifier());
							}
							break;
						}
					} else {
						switch (msg.getEventType()) {
						case Notification.ADD_MANY:
							for (Iterator<?> iter = ((Collection) msg.getNewValue())
									.iterator(); iter.hasNext();) {
								Object obj = iter.next();
								otherEnd = (EObject) obj;
								if (oppositeFeature.resolveOwner()) {
									otherEnd = (EObject) resolve(otherEnd);
									replace(feature, obj, otherEnd);
								}
								if (otherEnd != null) {
									OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
									EObject oldValue = (EObject) adapter
											.getOppositeFeatureMap().get(
													oppositeFeature);
									if (oldValue != null) {
										// remove otherEnd from target feature of
										// oldValue
										((Collection) oldValue
												.eGet((EStructuralFeature) f))
												.remove(otherEnd);
									}
									adapter.getOppositeFeatureMap().put(
											oppositeFeature, msg.getNotifier());
								}
							}
							break;
						case Notification.REMOVE_MANY:
							for (Iterator<?> iter = ((Collection) msg.getOldValue())
									.iterator(); iter.hasNext();) {
								otherEnd = (EObject) iter.next();
								if (oppositeFeature.resolveOwner()) {
									otherEnd = (EObject) resolve(otherEnd);
								}
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								adapter.getOppositeFeatureMap().put(
										oppositeFeature, null);
							}
							break;
						case Notification.ADD:
							otherEnd = (EObject) msg.getNewValue();
							if (oppositeFeature.resolveOwner()) {
								otherEnd = (EObject) resolve(otherEnd);
								replace(feature, msg.getNewValue(), otherEnd);
							}
							if (otherEnd != null) {
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								EObject oldValue = (EObject) adapter
										.getOppositeFeatureMap().get(
												oppositeFeature);
								if (oldValue != null) {
									// remove otherEnd from target feature of
									// oldValue
									((Collection) oldValue
											.eGet((EStructuralFeature) f))
											.remove(otherEnd);
								}
								adapter.getOppositeFeatureMap().put(
										oppositeFeature, msg.getNotifier());
							}
							break;
						case Notification.SET:
							otherEnd = (EObject) msg.getNewValue();
							if (oppositeFeature.resolveOwner()) {
								otherEnd = (EObject) resolve(otherEnd);
								replace(feature, msg.getNewValue(), otherEnd);
							}
							if (otherEnd != null) {
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								EObject oldValue = (EObject) adapter
										.getOppositeFeatureMap().get(
												oppositeFeature);
								if (oldValue != null) {
									// set the target feature of oldValue to null
									oldValue.eSet((EStructuralFeature) f, null);
								}
								adapter.getOppositeFeatureMap().put(
										oppositeFeature, msg.getNotifier());
							}
							else {
								EStructuralFeature targetFeature = (EStructuralFeature) f;
								if(!targetFeature.isMany()) {
									oldOtherEnd = (EObject) msg.getOldValue();
									if(oldOtherEnd != null) {
										OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(oldOtherEnd);
										adapter.getOppositeFeatureMap().put(oppositeFeature, null);
									}
								}
							}
							break;
						case Notification.REMOVE:
							// case Notification.UNSET:
							otherEnd = (EObject) msg.getOldValue();
							if (oppositeFeature.resolveOwner()) {
								otherEnd = (EObject) resolve(otherEnd);
							}
							if (otherEnd != null) {
								OppositeFeatureAdapter adapter = getOppositeFeatureAdapter(otherEnd);
								adapter.getOppositeFeatureMap().put(
										oppositeFeature, null);
							}
							break;

						}
					}
				}
			}
		}
		
		/**
		 * Resolves the given proxy object.
		 * 
		 * It does nothing right now, just returns the original object. Subclass to
		 * override.
		 * 
		 * @param object
		 *            a proxy object to resolve
		 * @return the resolved object
		 */
		protected Object resolve(Object object) {
			if (object instanceof InternalEObject
					&& ((InternalEObject) object).eIsProxy()) {
				return ((InternalEObject) getTarget()).eResolveProxy((InternalEObject) object);
			}
			return object;
		}

		@Override
		public void setTarget(Notifier newTarget) {
			if(target != null && newTarget != null) {
				throw new IllegalArgumentException("An OppositeFeatureAdapter cannot be associated with 2 targets at the same time.");
			}
			super.setTarget(newTarget);
		}
		
		private Map<OppositeFeature, Object> createOppositeFeatureMap() {
			Map<OppositeFeature, Object> map = new HashMap<OppositeFeature, Object>();
			for (Iterator<?> iter = OppositeFeature.classOppositeFeaturesMap
					.entrySet().iterator(); iter.hasNext();) {
				Map.Entry entry = (Map.Entry) iter.next();
				Class cls = (Class) entry.getKey();
				if (cls.isInstance(this)) {
					for (Iterator iterator = ((Collection) entry.getValue())
							.iterator(); iterator.hasNext();) {
						map.put((OppositeFeature) iterator.next(), null);
					}
				}
			}
			if (map.isEmpty()) {
				hasOppositeFeature = false;
				return Collections.emptyMap();
			}
			return map;
		}
		
		/**
		 * Gets the opposite feature map.
		 * 
		 * @return a map containing the opposite features mapped to their values
		 */
		protected Map<OppositeFeature, Object> getOppositeFeatureMap() {
			if (oppositeFeatureMap == null && hasOppositeFeature) {
				oppositeFeatureMap = createOppositeFeatureMap();
			}
			if(oppositeFeatureMap == null) {
				return Collections.emptyMap();
			}
			return oppositeFeatureMap;
		}

		protected void oppositeAdd(OppositeFeature oppositeFeature, Object object) {
			List list = (List) getOppositeFeatureMap().get(oppositeFeature);
			if (list == null) {
				list = new OppositeFeatureResolvingEList((EObject) getTarget(), oppositeFeature);
				getOppositeFeatureMap().put(oppositeFeature, list);
			}
			if (!list.contains(object)) {
				list.add(object);
			}
		}

		protected void oppositeRemove(OppositeFeature oppositeFeature, Object object) {
			List list = (List) getOppositeFeatureMap().get(oppositeFeature);
			if (list == null) {
				list = new OppositeFeatureResolvingEList((EObject) getTarget(), oppositeFeature);
				getOppositeFeatureMap().put(oppositeFeature, list);
			}
			list.remove(object);
		}

		private void replace(EStructuralFeature feature, Object oldValue, EObject newValue) {
			if (newValue != null && !newValue.eIsProxy() && newValue != oldValue) {
				EObject eObject = (EObject) getTarget();
				boolean notify = eObject.eDeliver();
				try {
					eObject.eSetDeliver(false);
					EcoreUtil.replace(eObject, feature, oldValue, newValue);
				} catch (Exception e) {
					if (DEBUG) {
						CommonPlugin.INSTANCE.log(e);
						e.printStackTrace();
						System.out.println("OppositeFeatureAdapter.replace():"); //$NON-NLS-1$
						System.out.println("  object: " + eObject); //$NON-NLS-1$
						System.out.println("  feature: " + feature); //$NON-NLS-1$
						System.out.println("  proxy: " + oldValue); //$NON-NLS-1$
						System.out.println("  resolved: " + newValue); //$NON-NLS-1$
					}
				} finally {
					eObject.eSetDeliver(notify);
				}
			}

		}

		public Collection<OppositeFeature> getOppositeFeatures() {
			return getOppositeFeatureMap().keySet();
		}
		
		public Object getOppositeFeatureValue(OppositeFeature feature) {
			Object value = getOppositeFeatureMap().get(feature);

			// System.out.println("MultiResourceEObject.getOppositeFeatureValue():");
			// System.out.println(" feature: " + feature);
			// System.out.println(" value: " + value);
			// System.out.println(" this: " + this);

			if (feature.isMany()) {
				if (value == null) {
					return Collections.EMPTY_LIST;
				}

				return ((OppositeFeatureResolvingEList) value)
						.getUnmodifiableList();
			} else if (value instanceof EObject
					&& ((EObject) value).eResource() == null) {
				getOppositeFeatureMap().put(feature, null);
				return null;
			}

			Object resolved = resolve(value);
			if (resolved != value) {
				getOppositeFeatureMap().put(feature, resolved);
				value = resolved;
			}
			return value;
		}

	}
}
