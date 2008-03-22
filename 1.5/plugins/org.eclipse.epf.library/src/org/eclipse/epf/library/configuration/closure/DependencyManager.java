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
package org.eclipse.epf.library.configuration.closure;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.ILibraryServiceListener;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.events.ILibraryChangeListener;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;

/**
 * Manages the method element dependencies in a method library.
 * 
 * @author Jinhua Xi
 * @author Kelvin Low
 * @since 1.0
 */
public class DependencyManager {

	// If true, generate debug traces.
	protected static boolean debug = LibraryPlugin.getDefault().isDebugging();

	// The method library.
	protected MethodLibrary library;

	// The library manager.
	protected ILibraryManager libraryManager;

	// A map of package elements to PackageDependency objects.
	protected Map dependencyMap = new HashMap();

	// A library change listener.
	private ILibraryChangeListener libListener = null;

	/**
	 * Creates a new instance.
	 */
	public DependencyManager(MethodLibrary library) {
		this.library = library;
		this.libraryManager = LibraryService.getInstance().getLibraryManager(
				library);
		init();
	}

	/**
	 * Performs the necessary initialization.
	 */
	protected void init() {
		
		libListener = new ILibraryChangeListener() {
			public void libraryChanged(int option, Collection changedItems) {
//				if (option == ILibraryChangeListener.OPTION_LOADED
//						|| option == ILibraryChangeListener.OPTION_CREATED) {
//					// TODO: Is this necessary?
//					// refresh();
//				} else 
				if (option == ILibraryChangeListener.OPTION_DELETED) {
					handleDeletedElement(changedItems);
				} else if (option == ILibraryChangeListener.OPTION_CHANGED
						|| option == ILibraryChangeListener.OPTION_NEWCHILD) {
					if (changedItems != null && changedItems.size() > 0) {
						for (Iterator it = changedItems.iterator(); it
								.hasNext();) {
							try {
								Object e = it.next();
								if (e instanceof MethodElement) {
									buildDependencyFor((MethodElement) e);
								} else {
									if (debug) {
										System.out.println(e
												+ " is not a method element"); //$NON-NLS-1$
									}
								}
							} catch (Exception e) {
								if (debug) {
									e.printStackTrace();
								}
							}
						}
					}
				}
			}
		};

		libraryManager.addListener(libListener);
	}

	/**
	 * Returns the package dependency for a method element.
	 * 
	 * @param element
	 *            A method element.
	 * @return A <code>PackageDependency</code>.
	 */
	public PackageDependency getDependency(MethodElement element) {
		if (!LibraryUtil.selectable(element)) {
			return null;
		}

		// Always rebuild the dependency for the element since the
		// dependents may not be fully established.
		PackageDependency dependency = getDependency(element, false);
		if (dependency == null || !dependency.isLoaded()) {
			buildDependency(element);
		}

		return getDependency(element, false);
	}

	/**
	 * Prints the dependency information.
	 */
	public void printDependency() {
		for (Iterator it = dependencyMap.values().iterator(); it.hasNext();) {
			((PackageDependency) it.next()).print();
		}
	}

	/**
	 * Builds the dependency for a method element.
	 * 
	 * @param element
	 *            A method element.
	 */
	private void buildDependency(MethodElement element) {
		if (element == null) {
			return;
		}

		try {
			PackageDependency dependency = buildDependencyFor(element);

			EList elements = element.eContents();
			if (elements != null) {
				for (Iterator it = elements.iterator(); it.hasNext();) {
					Object obj = it.next();
					if (! (obj instanceof MethodElement)) {
						continue;
					}
					MethodElement methodElement = (MethodElement) obj;
					if (methodElement != null
							&& !LibraryUtil.selectable(methodElement)) {
						buildDependencyFor(methodElement);
					}
				}
			}

			dependency.setLoaded(true);
		} catch (Exception e) {
			if (debug) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Builds the dependency for a method element.
	 * <p>
	 * This creates an <code>ElementReference</code> for the given element
	 * that points to all the referenced elements, and adds the given element as
	 * a dependent element to all the referenced elements. This should only be
	 * called once for each method element.
	 * 
	 * @param element
	 *            A method element.
	 * @return A <code>PackageDependency</code>.
	 */
	private PackageDependency buildDependencyFor(MethodElement element) {
		if (element == null) {
			return null;
		}

		// Build the dependency on the selectable element/parent only
		MethodElement selectableElement = (MethodElement)LibraryUtil.getSelectable(element);
		if (selectableElement == null) {
			return null;
		}

		PackageDependency dependency = getDependency(selectableElement, true);

		// Remove any existing element reference for this element.
		dependency.removeReference(element);
/*
		// Get the VariabilityElement.
		ContentElement baseElement = null;
		if (element instanceof ContentElement) {
			baseElement = (ContentElement) ((ContentElement) element)
					.getVariabilityBasedOnElement();
			if (baseElement != null) {
				// Establish the package reference.
				EObject selectableBase = LibraryUtil.getSelectable(baseElement);
				if (selectableBase != null) {
					PackageReference pkgRef = dependency.getReference(
							selectableBase, true);

					if (!pkgRef.hasReference(element, baseElement)) {
						// Add the detail element reference to the package
						// reference.
						VariabilityElementReference ref = new VariabilityElementReference(
								element, baseElement);
						pkgRef.addReference(ref);
					}

					// Set the dependent of the referenced package.
					getDependency(selectableBase, true).addDependent(
							selectableElement);
				}
			}
		}

		List references = element.eCrossReferences();

		// Update the dependents of those elements in the list.
		if (references != null && references.size() > 0) {
			// Get the selectable references
			for (Iterator it = references.iterator(); it.hasNext();) {
				EObject refElement = (EObject) it.next();
				EObject selectableRef = LibraryUtil.getSelectable(refElement);
				if (selectableRef != null) {
					PackageReference pkgRef = dependency.getReference(
							selectableRef, true);

					if (element == selectableElement
							&& refElement == selectableRef) {
						// No need to add this.
						continue;
					}

					if (!pkgRef.hasReference(element, refElement)) {
						GeneralReference ref = new GeneralReference(element,
								refElement);
						pkgRef.addReference(ref);
					}

					getDependency(selectableRef, true).addDependent(
							selectableElement);
				}
			}
		}
*/
		List properties = LibraryUtil.getStructuralFeatures(element);
		for (int i = 0; i < properties.size(); i++) {
			EStructuralFeature f = (EStructuralFeature) properties.get(i);
			if (!(f instanceof EReference) ) {
				continue;
			}
			
			EReference feature = (EReference)f;
			if ( feature.isContainer() || feature.isContainment() ) {
				continue;
			}
						
			Object value = element.eGet(feature);
			if ( value == null ) {
				continue;
			}
			
			MethodElement refElement = null;
			List values = null;
			int count = 0;
			
			if ( feature.isMany() ) {
				values = (List)value;
				if ( values.size() > 0 ) {
					refElement = (MethodElement)values.get(count);
				}
			} else if ( value instanceof MethodElement ) {
				refElement = (MethodElement)value;				
			}
			 
			while ( refElement != null ) {
				MethodElement selectableRef = (MethodElement)LibraryUtil.getSelectable(refElement);
				if (selectableRef != null) {
					PackageReference pkgRef = dependency.getReference(
							selectableRef, true);
	
					if (element == selectableElement
							&& refElement == selectableRef) {
						// No need to add this.
						break;
					}
	
					ElementReference ref = pkgRef.getReference(element, refElement);
					if (ref == null) {
						ref = new ElementReference(element, refElement);
						pkgRef.addReference(ref);
					}
	
					ref.addFeature(feature);
					
					getDependency(selectableRef, true).addDependent(
							selectableElement);
				}
				
				refElement = null;
				if ( values != null ) {
					count++;
					if ( count < values.size() ) {
						refElement = (MethodElement)values.get(count);
					}
				}
			}
		}
		
		
		return dependency;
	}

	private PackageDependency getDependency(MethodElement element, boolean create) {
		if (!LibraryUtil.selectable(element)) {
			if (debug) {
				System.out
						.println("Error, Selectable element required: " + element); //$NON-NLS-1$
			}
			return null;
		}

		PackageDependency dependency = (PackageDependency) dependencyMap
				.get(element);
		if (dependency == null && create) {
			dependency = new PackageDependency(element);
			dependencyMap.put(element, dependency);
		}

		return dependency;

	}

	private void handleDeletedElement(Collection changedItems) {
		for (Iterator it = changedItems.iterator(); it.hasNext();) {
			Object element = it.next();
			if (element instanceof MethodElement) {
				removeReference((MethodElement) element);
			}
		}
	}

	/**
	 * Removes the element reference for the specified element, rebuild later
	 * 
	 * @param element
	 *            A method element.
	 */
	private void removeReference(MethodElement element) {
		PackageDependency dependency = null;

		MethodElement pkg = (MethodElement) LibraryUtil.getSelectable(element);
		if ((pkg == null) || ((dependency = getDependency(pkg, false)) == null)) {
			return;
		}

		dependency.removeReference(element);
	}

}
