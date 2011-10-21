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
package org.eclipse.epf.library.ui.actions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.library.ILibraryServiceListener;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.util.Comparators;
import org.eclipse.epf.library.prefs.PreferenceUtil;
import org.eclipse.epf.library.ui.LibraryUIResources;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

/**
 * Contributes a method configuration comboxbox to the system toolbar.
 * 
 * @author Kelvin Low
 * @author Bingxue Xu
 * @author Jinhua Xi
 * @since 1.0
 */
public class ConfigurationContributionItem extends ContributionItem {

	private static Combo configCombo;

	private static ComboViewer configComboViewer;

	protected ToolItem item;

	protected CoolItem coolItem;

	protected ILibraryServiceListener libSvcListener;

	// The content provider.
	protected IStructuredContentProvider contentProvider;

	// The label provider.
	protected ILabelProvider labelProvider;
	
	protected ISelectionChangedListener postSelectionChangedListener = new ISelectionChangedListener() {
		public void selectionChanged(SelectionChangedEvent event) {
			performSelectionChanged(getCollapseConfigViewAction());
		}
	};

	/**
	 * Creates a new instance.
	 */
	public ConfigurationContributionItem(IAction action) {
		super();
	}

	/*
	 * @see org.eclipse.jface.action.ContributionItem#fill(org.eclipse.swt.widgets.ToolBar,
	 *      int)
	 */
	public void fill(ToolBar parent, int index) {
		item = new ToolItem(parent, SWT.SEPARATOR);
		Control box = createControl(parent);
		item.setControl(box);
		item.setWidth(240);
	}

	/*
	 * @see org.eclipse.jface.action.ContributionItem#fill(org.eclipse.swt.widgets.CoolBar,
	 *      int)
	 */
	public void fill(CoolBar coolBar, int index) {
		Control box = createControl(coolBar);

		if (index >= 0) {
			coolItem = new CoolItem(coolBar, SWT.DROP_DOWN, index);
		} else {
			coolItem = new CoolItem(coolBar, SWT.DROP_DOWN);
		}

		// Set the back reference.
		coolItem.setData(this);

		// Add the toolbar to the CoolItem widget.
		coolItem.setControl(box);

		// If the toolbar item exists then adjust the size of the cool item.
		Point toolBarSize = box.computeSize(SWT.DEFAULT, SWT.DEFAULT);

		// Set the preferred size to the size of the toolbar plus trim.
		coolItem.setMinimumSize(toolBarSize);
		coolItem.setPreferredSize(toolBarSize);
		coolItem.setSize(toolBarSize);
	}

	/*
	 * @see org.eclipse.jface.action.ContributionItem#fill(org.eclipse.swt.widgets.Composite)
	 */
	public void fill(Composite parent) {
		createControl(parent);
	}

	/**
	 * Creates the control.
	 */
	private Control createControl(Composite parent) {
		configCombo = new Combo(parent, SWT.DROP_DOWN | SWT.READ_ONLY);
		configCombo.setVisibleItemCount(10);
		configCombo.setEnabled(true);
		configComboViewer = new ComboViewer(configCombo);

		contentProvider = new AdapterFactoryContentProvider(
				TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory()) {
			public Object[] getElements(Object object) {
				List<Object> configsList = new ArrayList<Object>();
				configsList.addAll(Arrays.asList(LibraryServiceUtil
						.getMethodConfigurations(LibraryService.getInstance()
								.getCurrentMethodLibrary())));
				Collections.sort(configsList, Comparators.DEFAULT_COMPARATOR);
				if (LibraryService.getInstance()
						.getCurrentMethodConfiguration() == null) {
					configsList.add(0,
							LibraryUIResources.selectConfigLabel_text);
				}
				return configsList.toArray();
			}
		};

		labelProvider = new AdapterFactoryLabelProvider(
				TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory()) {
			public String getText(Object object) {
				if (object instanceof String) {
					return (String) object;
				} else if (object instanceof MethodConfiguration) {
					return ((MethodConfiguration) object).getName();
				} else
					return object.toString();
			}
		};

		configComboViewer.setContentProvider(contentProvider);
		configComboViewer.setLabelProvider(labelProvider);
		configComboViewer.setInput(LibraryService.getInstance()
				.getCurrentMethodLibrary());

		String savedConfigName = PreferenceUtil.getSavedLastConfig();
		MethodConfiguration savedConfig = LibraryServiceUtil
				.getMethodConfiguration(LibraryService.getInstance()
						.getCurrentMethodLibrary(), savedConfigName);
		
		if (savedConfig != null) {
			configComboViewer
					.setSelection(new StructuredSelection(savedConfig));
		} else {
			configComboViewer.setSelection(new StructuredSelection(
					LibraryUIResources.selectConfigLabel_text));
		}

		configComboViewer
			.addPostSelectionChangedListener(postSelectionChangedListener);

		configComboViewer.getControl().addDisposeListener(
				new DisposeListener() {
					public void widgetDisposed(DisposeEvent event) {
						dispose();
					}
				});


		libSvcListener = new ILibraryServiceListener() {

			public void configurationSet(MethodConfiguration config) {
				configComboViewer.removePostSelectionChangedListener(postSelectionChangedListener);
				try {
					selectConfiguration(config);
				}
				finally {
					configComboViewer
					.addPostSelectionChangedListener(postSelectionChangedListener);
				}
				
			}

			public void libraryClosed(MethodLibrary library) {
				configComboViewer.setInput(null);
			}

			public void libraryCreated(MethodLibrary library) {
				configComboViewer.setInput(library);
				selectConfiguration(null);
			}

			public void libraryOpened(MethodLibrary library) {
				configComboViewer.setInput(library);
				refresh();
				MethodConfiguration config = LibraryService.getInstance().getCurrentMethodConfiguration();					
				configComboViewer.setSelection(new StructuredSelection(
						config != null ? config : LibraryUIResources.selectConfigLabel_text), true);

			}

			public void libraryReopened(MethodLibrary library) {
				if (library != configComboViewer.getInput()) {
					configComboViewer.setInput(library);
					refresh();
				}
			}

			public void librarySet(MethodLibrary library) {
				if (library != configComboViewer.getInput()) {
					configComboViewer.setInput(library);
					if (library == null) {
						selectConfiguration(null);
					} else {
						refresh();
					}
				}
			}

		};
		LibraryService.getInstance().addListener(libSvcListener);

		return configCombo;
	}

	/*
	 * @see org.eclipse.jface.action.ContributionItem#void setVisibile(boolean)
	 */
	public void setVisible(boolean visible) {
		if (visible) {
			LibraryService.getInstance().addListener(libSvcListener);
		} else {
			LibraryService.getInstance().removeListener(libSvcListener);
		}
		super.setVisible(visible);
	}

	/**
	 * Returns the currently selected method configuration
	 */
	private static MethodConfiguration getSelectedConfig() {
		IStructuredSelection selection = (IStructuredSelection) configComboViewer
				.getSelection();
		Object object = selection.getFirstElement();
		if (object instanceof MethodConfiguration) {
			return (MethodConfiguration) object;
		}
		return null;
	}

	/**
	 * Return currently selected method configuration name
	 */
	private static String getCurrentSelectedConfigName() {
		IStructuredSelection selection = (IStructuredSelection) configComboViewer
				.getSelection();
		Object object = selection.getFirstElement();
		if (object != null && object instanceof MethodConfiguration) {
			return ((MethodConfiguration) object).getName();
		}
		if (object instanceof String)
			return (String) object;
		return ""; //$NON-NLS-1$
	}

	private static void performSelectionChanged(IAction collapseConfigViewAction) {
		if (LibraryService.getInstance().getCurrentMethodLibrary() != null) {
			MethodConfiguration config = getSelectedConfig();
			if (config != LibraryService.getInstance()
					.getCurrentMethodConfiguration()) {
				if (collapseConfigViewAction != null) {
					collapseConfigViewAction.run();
				}
				LibraryService.getInstance().setCurrentMethodConfiguration(config);
			}
			PreferenceUtil.saveSelectedConfigIntoPersistence(getCurrentSelectedConfigName());
			refresh();
		}
	}

	private IAction collapseConfigViewAction;
	
	private IAction getCollapseConfigViewAction() {
		return collapseConfigViewAction;
	}

	public void setCollapseConfigViewAction(IAction collapseConfigViewAction) {
		this.collapseConfigViewAction = collapseConfigViewAction;
	}

	/*
	 * @see org.eclipse.jface.action.ContributionItem#dispose()
	 */
	public void dispose() {
		if (libSvcListener != null) {
			LibraryService.getInstance().removeListener(libSvcListener);
		}
		
		if (configComboViewer != null) {
			configComboViewer
			.removePostSelectionChangedListener(postSelectionChangedListener);
		}
		
		super.dispose();
	}

	/**
	 * Refreshes the configuration combo.
	 */
	public static void refresh() {
		configComboViewer.refresh();
	}

	/**
	 * Select a configuration in configuration combo box.
	 * 
	 * @param config
	 */
	private static void selectConfiguration(MethodConfiguration config) {
		//NameCache.getInstance().clear();
		if (config != null && config == getSelectedConfig()) {
			return;
		}
		if (config != null) {
			configComboViewer.setSelection(new StructuredSelection(config));
		} else {
			// Perform a refresh to add the "Select a configuration"
			// string to the combo.
			refresh();
			configComboViewer.setSelection(new StructuredSelection(
					LibraryUIResources.selectConfigLabel_text), true);
		}
	}

	public void setEnabled(boolean enabled) {
		configCombo.setEnabled(enabled);
	}
}