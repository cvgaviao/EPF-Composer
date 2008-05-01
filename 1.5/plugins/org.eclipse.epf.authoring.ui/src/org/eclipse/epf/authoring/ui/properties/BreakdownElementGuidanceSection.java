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
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.dialogs.ItemsFilterDialog;
import org.eclipse.epf.authoring.ui.filters.ProcessGuidanceFilter;
import org.eclipse.epf.diagram.model.util.DiagramInfo;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.configuration.GuidanceItemProvider;
import org.eclipse.epf.library.edit.itemsfilter.FilterConstants;
import org.eclipse.epf.library.edit.itemsfilter.FilterInitializer;
import org.eclipse.epf.library.edit.process.IBSItemProvider;
import org.eclipse.epf.library.edit.process.command.AddGuidanceToBreakdownElementCommand;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.Concept;
import org.eclipse.epf.uma.Example;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.Guideline;
import org.eclipse.epf.uma.ReusableAsset;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * The breakdown element guidance section. It list all available guidances for an breakdown element
 * 
 * @author Shilpa Toraskar
 * @since 1.0
 * 
 */
public class BreakdownElementGuidanceSection extends AbstractSection {
	protected ILabelProvider labelProvider = new AdapterFactoryLabelProvider(
			TngAdapterFactory.INSTANCE
					.getNavigatorView_ComposedAdapterFactory());

	private FormToolkit toolkit;

	private Button ctrl_add_1, ctrl_remove_1;

	private Table ctrl_table_1;

	private TableViewer viewer_1;

	// element
	private BreakdownElement element;

	// action manager
	private IActionManager actionMgr;

	public final String tabName = FilterConstants.GUIDANCE;

	private IFilter generalGuidanceFilter = null;

	

	/**
	 * Get General guidance filter
	 * 
	 */
	public IFilter getGeneralGuidanceFilter() {
		if (generalGuidanceFilter == null) {
			generalGuidanceFilter = new ProcessGuidanceFilter(
					getConfiguration(), null, tabName) {
				public boolean childAccept(Object obj) {
					if (super.childAccept(obj))
						return true;
					if (obj instanceof GuidanceItemProvider) {
						if (((GuidanceItemProvider) obj).getChildren(obj)
								.isEmpty())
							return false;
						else
							return true;
					}
					Class cls = FilterInitializer.getInstance()
							.getClassForType(helper.getFilterTypeStr());
					if (cls != null) {
						if (cls.isInstance(obj)) {
							return true;
						} else {
							return false;
						}
					}
					if ((obj instanceof Checklist) || (obj instanceof Concept)
							|| (obj instanceof Example)
							|| (obj instanceof Guideline)
							|| (obj instanceof ReusableAsset)
							|| (obj instanceof SupportingMaterial))
						return true;

					return false;

				}
			};
		}else {
			((ProcessGuidanceFilter) generalGuidanceFilter).setMethodConfiguration(getConfiguration());
		}
		return generalGuidanceFilter;
	}

	

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.AbstractSection#createControls(org.eclipse.swt.widgets.Composite, org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	public void createControls(Composite parent,
			TabbedPropertySheetPage tabbedPropertySheetPage) {

		super.createControls(parent, tabbedPropertySheetPage);
		init();

		parent.setLayout(new GridLayout());
		parent.setLayoutData(new GridData(GridData.FILL_BOTH));

		// create guidance section
		createGuidanceSection(parent);

		// add listeners
		addListeners();

		// update controls
		updateControls();

	}

	/**
	 * Initialize
	 */
	private void init() {
		// get activity object
		element = (BreakdownElement) getElement();

		// get toolkit
		toolkit = getWidgetFactory();

		// get action manager
		actionMgr = EPFPropertySheetPage.getActionManager();
	}


	/**
	 *  Update controls based on editable flag. Controls can become editable or un-editable
	 */
	public void updateControls() {
		ctrl_add_1.setEnabled(editable);
		
		IStructuredSelection selection = (IStructuredSelection) viewer_1
				.getSelection();
		if (selection.size() > 0 && editable) {
			ctrl_remove_1.setEnabled(true);
		} else {
			ctrl_remove_1.setEnabled(false);
		}
	}


	/**
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	public void refresh() {
		try {
			if (getElement() instanceof BreakdownElement) {
				element = (BreakdownElement) getElement();

				viewer_1.refresh();

				// hide/show controls
				updateControls();
			}

		} catch (Exception ex) {
			logger.logError("Error refreshing activity guidance section: ", ex); //$NON-NLS-1$
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.properties.AbstractSection#dispose()
	 */
	public void dispose() {
		super.dispose();

		if (labelProvider != null) {
			labelProvider.dispose();
		}
	}

	/**
	 * Create guidance section on the given composite
	 * @param composite
	 */
	private void createGuidanceSection(Composite composite) {
		int tableHeight = 50;
		String sectionTitle = null;
		String sectionDesc = null;
		String tableTitle = null;

		// GENERAL GUIDANCE
		{
			sectionTitle = PropertiesResources.Activity_GeneralGuidanceTitle; 
			sectionDesc = PropertiesResources.Activity_GeneralGuidanceDescription; 
			tableTitle = PropertiesResources.Activity_Selected_GeneralGuidance; 

			Section section = FormUI.createSection(toolkit, composite,
					sectionTitle, sectionDesc);

			// create composite
			Composite sectionComposite = FormUI.createComposite(toolkit,
					section, 2, false);

			Composite pane1 = FormUI.createComposite(toolkit, sectionComposite,
					GridData.FILL_BOTH);
			FormUI.createLabel(toolkit, pane1, tableTitle);

			ctrl_table_1 = FormUI.createTable(toolkit, pane1, tableHeight);
			viewer_1 = new TableViewer(ctrl_table_1);
			IStructuredContentProvider contentProvider = new AdapterFactoryContentProvider(
					getAdapterFactory()) {
				public Object[] getElements(Object object) {
					return getFilteredList(getSelectedGuidances()).toArray();
				}
			};

			viewer_1.setContentProvider(contentProvider);
			viewer_1.setLabelProvider(labelProvider);
			viewer_1.setInput(element);

			// create buttons for table2
			Composite pane2 = FormUI.createComposite(toolkit, sectionComposite,
					GridData.VERTICAL_ALIGN_CENTER
							| GridData.HORIZONTAL_ALIGN_CENTER);

			ctrl_add_1 = FormUI.createButton(toolkit, pane2,
					PropertiesResources.Process_Add);
			ctrl_remove_1 = FormUI.createButton(toolkit, pane2,
					PropertiesResources.Process_Remove);

			toolkit.paintBordersFor(pane1);
		}
	}

	/**
	 * Add listeners
	 * 
	 */
	private void addListeners() {
		ItemProviderAdapter adapter = (ItemProviderAdapter) getAdapter();
		Object parent = null;
		if (adapter instanceof IBSItemProvider) {
			IBSItemProvider bsItemProvider = (IBSItemProvider) adapter;
			parent = bsItemProvider.getTopItem();
		} else {
			logger
					.logError("ActivityGuidanceSection::addListeners - IBSItemProvider is null"); //$NON-NLS-1$
			return;
		}

		{
			ctrl_table_1.addFocusListener(new FocusAdapter() {
				public void focusGained(FocusEvent e) {
					IStructuredSelection selection = (IStructuredSelection) viewer_1
							.getSelection();
					if ((selection.size() > 0) && editable)
						ctrl_remove_1.setEnabled(true);
				}
			});

			viewer_1
					.addSelectionChangedListener(new ISelectionChangedListener() {
						public void selectionChanged(SelectionChangedEvent event) {
							IStructuredSelection selection = (IStructuredSelection) viewer_1
									.getSelection();
							if ((selection.size() > 0) && editable)
								ctrl_remove_1.setEnabled(true);
						}
					});

			ctrl_add_1.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					IFilter filter = getGeneralGuidanceFilter();
					ItemsFilterDialog fd = new ItemsFilterDialog(PlatformUI
							.getWorkbench().getActiveWorkbenchWindow()
							.getShell(), filter, element,
							FilterConstants.GUIDANCE, getSelectedGuidances());

					fd.setTitle(FilterConstants.GUIDANCE);
					fd.setInput(UmaUtil.getMethodLibrary((EObject) element));
					fd.setBlockOnOpen(true);
					fd.setTypes(getFilterTypes());				
					fd.open();
					addGuidances(fd.getSelectedItems());
					viewer_1.refresh();
				}
			});

			ctrl_remove_1.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					IStructuredSelection selection = (IStructuredSelection) viewer_1
							.getSelection();
					if (selection.size() > 0) {
						// update the model
						ArrayList rmItems = new ArrayList();
						rmItems.addAll(selection.toList());
						removeGuidances(rmItems);
						viewer_1.refresh();

						// clear the selection
						viewer_1.setSelection(null, true);
					}
					ctrl_remove_1.setEnabled(false);
				}
			});
		}
	}

	/**
	 * Add guidances to the element
	 * @param addItems
	 * 				list of guidances to add
	 */
	private void addGuidances(List<Guidance> addItems) {
		// update the model
		AddGuidanceToBreakdownElementCommand command = new AddGuidanceToBreakdownElementCommand(
				element, addItems);
		actionMgr.execute(command);
	}

	/**
	 * Remove guidances from the element
	 * @param rmItems
	 * 				list of guidances to remove
	 */
	private void removeGuidances(List<Guidance> rmItems) {
		// update the model
		if (!rmItems.isEmpty()) {
			for (Iterator it = rmItems.iterator(); it.hasNext();) {
				Guidance item = (Guidance) it.next();

				// guidances for activity
				if (item instanceof Checklist) {
					actionMgr.doAction(IActionManager.REMOVE, element,
							UmaPackage.eINSTANCE.getBreakdownElement_Checklists(),
							item, -1);
				} else if (item instanceof Concept) {
					actionMgr.doAction(IActionManager.REMOVE, element,
							UmaPackage.eINSTANCE.getBreakdownElement_Concepts(), item,
							-1);
				} else if (item instanceof Example) {
					actionMgr.doAction(IActionManager.REMOVE, element,
							UmaPackage.eINSTANCE.getBreakdownElement_Examples(), item,
							-1);
				} else if (item instanceof SupportingMaterial) {
					actionMgr.doAction(IActionManager.REMOVE, element,
							UmaPackage.eINSTANCE
									.getBreakdownElement_SupportingMaterials(), item,
							-1);
				} else if (item instanceof Guideline) {
					actionMgr.doAction(IActionManager.REMOVE, element,
							UmaPackage.eINSTANCE.getBreakdownElement_Guidelines(),
							item, -1);
				} else if (item instanceof ReusableAsset) {
					actionMgr.doAction(IActionManager.REMOVE, element,
							UmaPackage.eINSTANCE.getBreakdownElement_ReusableAssets(),
							item, -1);
				} else {
					logger
							.logError("Can't remove Guidance: " + item.getType().getName() + ":" + item.getName()); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	
	/**
	 * Get selected guidances
	 * @return
	 * 			list of existing selected guidances
	 */
	private List<Guidance> getSelectedGuidances() {
		List<Guidance> itemList = new ArrayList();

		itemList.addAll(element.getChecklists());
		itemList.addAll(element.getConcepts());
		itemList.addAll(element.getExamples());
		itemList.addAll(element.getGuidelines());
		itemList.addAll(element.getReusableAssets());
		itemList.addAll(element.getSupportingMaterials());

		return itemList;
	}

	/**
	 * Return list of filter types
	 */
	protected String[] getFilterTypes() {
		String[] str = new String[14];
		int i = 0;
		str[i++] = FilterConstants.GUIDANCE;
		str[i++] = FilterConstants.space + FilterConstants.CHECKLISTS;
		str[i++] = FilterConstants.space + FilterConstants.CONCEPTS;
		str[i++] = FilterConstants.space
				+ FilterConstants.ESTIMATE_CONSIDERATIONS;
		str[i++] = FilterConstants.space + FilterConstants.EXAMPLES;
		str[i++] = FilterConstants.space + FilterConstants.GUIDELINES;
		str[i++] = FilterConstants.space + FilterConstants.PRACTICES;
		str[i++] = FilterConstants.space + FilterConstants.REPORTS;
		str[i++] = FilterConstants.space + FilterConstants.REUSABLE_ASSETS;
		str[i++] = FilterConstants.space + FilterConstants.SUPPORTING_MATERIALS;
		str[i++] = FilterConstants.space + FilterConstants.TEMPLATES;
		str[i++] = FilterConstants.space + FilterConstants.TOOL_MENTORS;
		str[i++] = FilterConstants.space + FilterConstants.WHITE_PAPERS;
		str[i++] = FilterConstants.space + FilterConstants.TERM_DEFINITIONS;
		return str;
	}
}