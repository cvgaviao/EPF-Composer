//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.search.ui.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.edit.ui.provider.ExtendedImageRegistry;
import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.search.ui.SearchUIResources;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.ContentElement;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.Discipline;
import org.eclipse.epf.uma.Domain;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessElement;
import org.eclipse.epf.uma.ProcessPackage;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;

/**
 * The content provider for the method search result tree viewer.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class SearchResultTreeContentProvider implements ITreeContentProvider {

	private static final String PKG_NAME_CONTENT = "Content"; //$NON-NLS-1$

	private static final String PKG_NAME_CORE_CONTENT = "CoreContent"; //$NON-NLS-1$

	private static final String PKG_NAME_CATEGORIES = "Categories"; //$NON-NLS-1$

	private static final String PKG_NAME_DISCIPLINES = "Disciplines"; //$NON-NLS-1$

	private static final String PKG_NAME_DOMAINS = "Domains"; //$NON-NLS-1$

	private static final String PKG_NAME_WORK_PRODUCT_TYPES = "WP Types"; //$NON-NLS-1$

	private static final String PKG_NAME_ROLESETS = "RoleSets"; //$NON-NLS-1$

	private static final String PKG_NAME_TOOLS = "Tools"; //$NON-NLS-1$

	private static final String PKG_NAME_CUSTOM_CATEGORIES = "CustomCategories"; //$NON-NLS-1$

	private static final String PKG_NAME_CAPABILITY_PATTERNS = "CapabilityPatterns"; //$NON-NLS-1$

	private static final String PKG_NAME_DELIVERY_PROCESSES = "DeliveryProcesses"; //$NON-NLS-1$

	private static final String METHOD_CONTENT = SearchUIResources.searchResult_methodContent; 

	private static final String CONTENT_PACKAGES = SearchUIResources.searchResult_contentPackages; 

	private static final String STANDARD_CATEGORIES = SearchUIResources.searchResult_standardCategories; 

	private static final String DISCIPLINES = SearchUIResources.searchResult_disciplines; 

	private static final String DOMAINS = SearchUIResources.searchResult_domains; 

	private static final String WORK_PRODUCT_TYPES = SearchUIResources.searchResult_workProductTypes; 

	private static final String ROLESETS = SearchUIResources.searchResult_roleSets; 

	private static final String TOOLS = SearchUIResources.searchResult_tools; 

	private static final String CUSTOM_CATEGORIES = SearchUIResources.searchResult_customCategories; 

	private static final String PROCESSES = SearchUIResources.searchResult_processes; 

	private static final String CAPABILITY_PATTERNS = SearchUIResources.searchResult_capabilityPatterns; 

	private static final String DELIVERY_PROCESSES = SearchUIResources.searchResult_deliveryProcesses; 

	private static final Object[] EMPTY_LIST = new Object[0];

	private TreeViewer treeViewer;

	private MethodSearchResult searchResult;

	private Map<Object, Object> elementMap;

	/**
	 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(Object)
	 */
	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof MethodSearchResult) {
			Object[] elements = ((MethodSearchResult) inputElement)
					.getElements();
			if (elements.length == 0) {
				return EMPTY_LIST;
			}
			elementMap = new HashMap<Object, Object>();
			for (int i = 0; i < elements.length; i++) {
				insert(elements[i], false);
			}

			insertUIFolders(inputElement);
		}
		return getChildren(inputElement);
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		if (newInput != null && newInput instanceof MethodSearchResult) {
			treeViewer = (TreeViewer) viewer;
			searchResult = (MethodSearchResult) newInput;
		}
	}

	public void elementsChanged(Object[] updatedElements) {
		for (int i = 0; i < updatedElements.length; i++) {
			if (searchResult.getMatchCount(updatedElements[i]) > 0) {
				if (treeViewer.testFindItem(updatedElements[i]) != null) {
					insert(updatedElements[i], true);
				} else {
					remove(updatedElements[i], true);
				}
			} else {
				treeViewer.remove(updatedElements[i]);
			}
		}
		treeViewer.refresh();
	}

	/**
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(Object)
	 */
	public Object[] getChildren(Object parentElement) {
		if (elementMap == null) {
			return EMPTY_LIST;
		}
		Set children = (Set) elementMap.get(parentElement);
		if (children == null) {
			return EMPTY_LIST;
		}
		return children.toArray();
	}

	/**
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(Object)
	 */
	public Object getParent(Object element) {
		if (element instanceof MethodPlugin) {
			return null;
		} else if (element instanceof SearchResultUIFolder) {
			return ((SearchResultUIFolder) element).getParent();
		} else if (element instanceof ProcessPackage) {
			ProcessPackage processPackage = (ProcessPackage) element;
			Object parent = processPackage.eContainer();
			if (parent instanceof ProcessComponent) {
				ProcessComponent processComponent = (ProcessComponent) parent;
				return processComponent.getProcess();
			} else {
				return parent;
			}
		} else if (element instanceof MethodPackage) {
			MethodPackage methodPackage = (MethodPackage) element;
			Object parent = methodPackage.eContainer();
			if (parent instanceof MethodPlugin) {
				return parent;
			}
			MethodPackage parentPackage = methodPackage.getParentPackage();
			return parentPackage;
		} else if (element instanceof Artifact) {
			Artifact artifact = (Artifact) element;
			Artifact containerArtifact = artifact.getContainerArtifact();
			if (containerArtifact != null) {
				return containerArtifact;
			} else {
				return UmaUtil.getContentPackage(artifact);
			}
		} else if (element instanceof Practice) {
			Practice practice = (Practice) element;
			return practice.getContainer();
		} else if (element instanceof Discipline) {
			Discipline discipline = (Discipline) element;
			return discipline.getContainer();			
		} else if (element instanceof Domain) {
			Domain domain = (Domain) element;
			return domain.getContainer();
		} else if (element instanceof ContentElement) {
			return UmaUtil.getContentPackage((ContentElement) element);
		} else if (element instanceof Process) {
			Process process = (Process) element;
			ProcessPackage parent = (ProcessPackage) process.eContainer();
			if (parent != null) {
				return parent.eContainer();
			}
		} else if (element instanceof Activity) {
			ProcessPackage processPackage = UmaUtil
					.getProcessPackage((ProcessElement) element);
			if (processPackage != null) {
				ProcessPackage parentPackage = (ProcessPackage) processPackage
						.getParentPackage();
				if (parentPackage != null) {
					if (parentPackage instanceof ProcessComponent) {
						ProcessComponent processComponent = (ProcessComponent) parentPackage;
						return processComponent.getProcess();
					}
					List processElements = parentPackage.getProcessElements();
					for (Iterator i = processElements.iterator(); i.hasNext();) {
						ProcessElement processElement = (ProcessElement) i
								.next();
						if (processElement instanceof Activity
								&& processElement.getName().equals(
										parentPackage.getName())) {
							return processElement;
						}
					}
				}
			}
		} else if (element instanceof ProcessElement) {
			ProcessPackage processPackage = UmaUtil
					.getProcessPackage((ProcessElement) element);
			if (processPackage != null) {
				if (processPackage instanceof ProcessComponent) {
					ProcessComponent processComponent = (ProcessComponent) processPackage;
					return processComponent.getProcess();
				}
				List processElements = processPackage.getProcessElements();
				for (Iterator i = processElements.iterator(); i.hasNext();) {
					ProcessElement processElement = (ProcessElement) i.next();
					if (processElement instanceof Activity
							&& processElement.getName().equals(
									processPackage.getName())) {
						return processElement;
					}
				}
				return processPackage;
			}
		}
		return null;
	}

	/**
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(Object)
	 */
	public boolean hasChildren(Object element) {
		return getChildren(element).length > 0;
	}

	public void clear() {
		treeViewer.refresh();
	}

	public void dispose() {
	}

	protected void insert(Object child, boolean refreshViewer) {
		Object parent = getParent(child);
		while (parent != null) {
			if (insertChild(parent, child)) {
				if (refreshViewer) {
					treeViewer.add(parent, child);
				}
			} else {
				if (refreshViewer) {
					treeViewer.refresh(parent);
				}
				return;
			}
			child = parent;
			parent = getParent(child);
		}
		if (insertChild(searchResult, child)) {
			if (refreshViewer) {
				treeViewer.add(searchResult, child);
			}
		}
	}

	protected boolean insertChild(Object parent, Object child) {
		Set children = (Set) elementMap.get(parent);
		if (children == null) {
			children = new HashSet();
			elementMap.put(parent, children);
		}
		return children.add(child);
	}

	protected void replace(Object parent, Object child, Object newChild) {
		insert(newChild, false);
		elementMap.put(newChild, (Set) elementMap.get(child));
		elementMap.remove(child);
		Set children = (Set) elementMap.get(parent);
		children.remove(child);
	}

	protected void remove(Object element, boolean refreshViewer) {
		if (hasChildren(element)) {
			if (refreshViewer) {
				treeViewer.refresh(element);
			}
		} else {
			if (searchResult.getMatchCount(element) == 0) {
				elementMap.remove(element);
				Object parent = getParent(element);
				if (parent != null) {
					removeFromSiblings(element, parent);
					remove(parent, refreshViewer);
				} else {
					removeFromSiblings(element, searchResult);
					if (refreshViewer) {
						treeViewer.refresh();
					}
				}
			} else {
				if (refreshViewer) {
					treeViewer.refresh(element);
				}
			}
		}
	}

	protected void removeFromSiblings(Object element, Object parent) {
		Set siblings = (Set) elementMap.get(parent);
		if (siblings != null) {
			siblings.remove(element);
		}
	}

	/**
	 * Inserts the UI folders into the search result tree.
	 */
	protected void insertUIFolders(Object inputElement) {
		Object[] elements = getChildren(inputElement);
		for (int i = 0; i < elements.length; i++) {
			Object element = elements[i];
			if (element instanceof MethodPlugin) {
				SearchResultUIFolder methodContentFolder = new SearchResultUIFolder(
						METHOD_CONTENT, ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
								.getImage("full/obj16/Content")), element); //$NON-NLS-1$
				SearchResultUIFolder processesFolder = new SearchResultUIFolder(
						PROCESSES, ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
								.getImage("full/obj16/Processes")), element); //$NON-NLS-1$
				Object[] methodPackages = getChildren(element);
				for (int j = 0; j < methodPackages.length; j++) {
					Object methodPackage = methodPackages[j];
					if (methodPackage instanceof ContentPackage
							&& ((ContentPackage) methodPackage).getName()
									.equals(PKG_NAME_CONTENT)) {
						Object[] packages = getChildren(methodPackage);
						for (int k = 0; k < packages.length; k++) {
							Object pkg = packages[k];
							if (pkg instanceof ContentPackage) {
								if (((ContentPackage) pkg).getName().equals(
										PKG_NAME_CORE_CONTENT)) {
									replace(
											element,
											pkg,
											new SearchResultUIFolder(
													CONTENT_PACKAGES,
													ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
															.getImage("full/obj16/MethodPackages")), methodContentFolder)); //$NON-NLS-1$
								} else if (((ContentPackage) pkg).getName()
										.equals(PKG_NAME_CATEGORIES)) {
									SearchResultUIFolder standardCategoriesFolder = new SearchResultUIFolder(
											STANDARD_CATEGORIES,
											ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
													.getImage("full/obj16/StandardCategories")), methodContentFolder); //$NON-NLS-1$
									SearchResultUIFolder customCategoriesFolder = new SearchResultUIFolder(
											CUSTOM_CATEGORIES,
											ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
													.getImage("full/obj16/CustomCategories")), methodContentFolder); //$NON-NLS-1$
									Object[] contentPackages = getChildren(pkg);
									for (int l = 0; l < contentPackages.length; l++) {
										Object contentPackage = contentPackages[l];
										if (contentPackage instanceof ContentPackage) {
											String pkgName = ((ContentPackage) contentPackage)
													.getName();
											if (pkgName
													.equals(PKG_NAME_CUSTOM_CATEGORIES)) {
												insert(customCategoriesFolder,
														false);
												elementMap
														.put(
																customCategoriesFolder,
																(Set) elementMap
																		.get(contentPackage));
											} else if (pkgName
													.equals(PKG_NAME_DISCIPLINES)) {
												insert(
														standardCategoriesFolder,
														false);
												replace(
														element,
														contentPackage,
														new SearchResultUIFolder(
																DISCIPLINES,
																ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
																		.getImage("full/obj16/Disciplines")), standardCategoriesFolder)); //$NON-NLS-1$
											} else if (pkgName
													.equals(PKG_NAME_DOMAINS)) {
												insert(
														standardCategoriesFolder,
														false);
												replace(
														element,
														contentPackage,
														new SearchResultUIFolder(
																DOMAINS,
																ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
																		.getImage("full/obj16/Domains")), standardCategoriesFolder)); //$NON-NLS-1$
											} else if (pkgName
													.equals(PKG_NAME_WORK_PRODUCT_TYPES)) {
												insert(
														standardCategoriesFolder,
														false);
												replace(
														element,
														contentPackage,
														new SearchResultUIFolder(
																WORK_PRODUCT_TYPES,
																ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
																		.getImage("full/obj16/WorkProductTypes")), standardCategoriesFolder)); //$NON-NLS-1$
											} else if (pkgName
													.equals(PKG_NAME_ROLESETS)) {
												insert(
														standardCategoriesFolder,
														false);
												replace(
														element,
														contentPackage,
														new SearchResultUIFolder(
																ROLESETS,
																ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
																		.getImage("full/obj16/Roles")), standardCategoriesFolder)); //$NON-NLS-1$
											} else if (pkgName
													.equals(PKG_NAME_TOOLS)) {
												insert(
														standardCategoriesFolder,
														false);
												replace(
														element,
														contentPackage,
														new SearchResultUIFolder(
																TOOLS,
																ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
																		.getImage("full/obj16/Tools")), standardCategoriesFolder)); //$NON-NLS-1$
											}
										}
									}
									elementMap.remove(pkg);
								}
							} else if (pkg instanceof ProcessPackage
									&& ((ProcessPackage) pkg).getName().equals(
											PKG_NAME_CAPABILITY_PATTERNS)) {
								replace(
										element,
										pkg,
										new SearchResultUIFolder(
												CAPABILITY_PATTERNS,
												ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
														.getImage("full/obj16/CapabilityPatterns")), processesFolder)); //$NON-NLS-1$
							}
						}
						Set children = (Set) elementMap.get(element);
						children.remove(methodPackage);
					} else if (methodPackage instanceof ProcessPackage
							&& ((ProcessPackage) methodPackage).getName()
									.equals(PKG_NAME_DELIVERY_PROCESSES)) {
						replace(
								element,
								methodPackage,
								new SearchResultUIFolder(
										DELIVERY_PROCESSES,
										ExtendedImageRegistry.getInstance().getImage(LibraryEditPlugin.INSTANCE
												.getImage("full/obj16/DeliveryProcesses")), processesFolder)); //$NON-NLS-1$
					}
				}
			}
		}
	}

}