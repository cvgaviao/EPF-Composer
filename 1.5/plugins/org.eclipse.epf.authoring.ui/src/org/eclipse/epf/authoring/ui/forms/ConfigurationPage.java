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
package org.eclipse.epf.authoring.ui.forms;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.edit.provider.IItemLabelProvider;
import org.eclipse.emf.edit.provider.IViewerNotification;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.emf.edit.ui.provider.ExtendedImageRegistry;
import org.eclipse.epf.authoring.ui.AuthoringUIHelpContexts;
import org.eclipse.epf.authoring.ui.AuthoringUIImages;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.editors.ConfigurationEditor;
import org.eclipse.epf.authoring.ui.editors.ConfigurationEditorInput;
import org.eclipse.epf.authoring.ui.providers.CategoryContentProvider;
import org.eclipse.epf.authoring.ui.providers.CategoryLabelProvider;
import org.eclipse.epf.authoring.ui.providers.CategoryTreeFilter;
import org.eclipse.epf.authoring.ui.util.AuthoringAccessibleListener;
import org.eclipse.epf.authoring.ui.util.ConfigurationMarkerHelper;
import org.eclipse.epf.authoring.ui.views.CategoryTreeViewerWrapper;
import org.eclipse.epf.authoring.ui.views.ConfigurationViewer;
import org.eclipse.epf.library.ILibraryManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationData;
import org.eclipse.epf.library.configuration.closure.ClosureListener;
import org.eclipse.epf.library.configuration.closure.ConfigurationClosure;
import org.eclipse.epf.library.configuration.closure.ElementDependencyError;
import org.eclipse.epf.library.configuration.closure.IConfigurationError;
import org.eclipse.epf.library.edit.IPluginUIPackageContextChangedListener;
import org.eclipse.epf.library.edit.PluginUIPackageContext;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.navigator.ContentItemProvider;
import org.eclipse.epf.library.edit.navigator.MethodPackagesItemProvider;
import org.eclipse.epf.library.edit.navigator.PluginUIPackagesItemProvider;
import org.eclipse.epf.library.edit.navigator.ProcessesItemProvider;
import org.eclipse.epf.library.edit.ui.UserInteractionHelper;
import org.eclipse.epf.library.edit.util.ConfigurationUtil;
import org.eclipse.epf.library.edit.util.MethodElementUtil;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.events.ILibraryChangeListener;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.ContentCategory;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

/**
 * The Configuration page in the Method Configuration editor.
 * 
 * @author Shashidhar Kannoori
 * @author Jinhua Xi
 * @author Shilpa Toraskar
 * @since 1.0
 */
public class ConfigurationPage extends FormPage implements ISelectionProvider {
	MethodConfiguration config = null;

	ConfigurationClosure closure = null;

	ConfigurationViewer treeViewer;

	ConfigPackageContentProvider contProvider;

	ISelectionChangedListener selectionChangedListener = null;
	
	protected CategoryTreeViewerWrapper addCategoryViewer;
	
	protected CategoryTreeViewerWrapper subCategoryViewer;
	
	protected CategoryTreeFilter addCategoryTreeFilter;

	protected CategoryTreeFilter subCategoryTreeFilter;

	ScrolledForm form = null;

	boolean isDirty = false;
	
	private boolean needUpdate = false;

	private String formPrefix = AuthoringUIResources.ConfigurationPage_FormPrefix; 

	// private EObject currentRootNode;
	private ArrayList expandedElements = new ArrayList();

	// private Text ctrl_name;

	// private Text ctrl_brief_desc;

	private List modifiedElements = new ArrayList();

	private Button closureButton;

	private Button fixWarningButton;

	private Button refreshButton;

	// private Button dependentButton;

	private Button hideButton;

	// private Text despText;

	private Text elemDespContentText;

	// private Text nameText;

	protected Collection selectionChangedListeners = new ArrayList();

	// private ISelectionChangedListener parentSelectionChangedListener = null;

	protected ISelection currentSelection = StructuredSelection.EMPTY;

	private static final ConfigurationMarkerHelper markerHelper = ConfigurationMarkerHelper.INSTANCE;

	ISelectionChangedListener msgViewListener = null;

	private ConfigTreeFilter configFilter;

	private IActionManager actionMgr;

	private ILibraryChangeListener libListener = null;

	protected Adapter configurationChangedListener = null;

	private static final ClosureListener closureListener = new ClosureListener() {
		@Override
		public void errorAdded(MethodConfiguration config, IConfigurationError error) {
			markerHelper.createMarker(config, error);
		}

		@Override
		public void errorRemoved(MethodConfiguration config, IConfigurationError error) {
			markerHelper.deleteMarker(config, error);
		}

		@Override
		public void errorUpdated(MethodConfiguration config, IConfigurationError error) {
			markerHelper.adjustMarker(null, config, error);
		}
	};

	private IPluginUIPackageContextChangedListener layoutListener = new IPluginUIPackageContextChangedListener() {
		public void layoutChanged(boolean isFlat) {
			refreshViewers();
		}
	};

	/**
	 * Creates an instance
	 * @param editor
	 */
	public ConfigurationPage(FormEditor editor) {
		super(
				editor,
				AuthoringUIResources.ConfigurationPage_Description1, AuthoringUIResources.ConfigurationPage_Description2); 
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	public void init(IEditorSite site, IEditorInput input) {
		setSite(site);
		setInput(input);

		ConfigurationEditorInput configInput = (ConfigurationEditorInput) input;
		config = configInput.getConfiguration();
		actionMgr = ((ConfigurationEditor) getEditor()).getActionManager();

		
		configurationChangedListener = new AdapterImpl() {
			public void notifyChanged(
					org.eclipse.emf.common.notify.Notification msg) {
				// System.out.println("$$$ catched modify config notification =
				// " + msg);
				int type = msg.getEventType();
				if (	type == org.eclipse.emf.common.notify.Notification.ADD
						|| type == org.eclipse.emf.common.notify.Notification.ADD_MANY
						|| type == org.eclipse.emf.common.notify.Notification.REMOVE
						||   type == org.eclipse.emf.common.notify.Notification.REMOVE_MANY) {					
					needUpdate = true;
					
//					This was done to refresh configuration plugin-package section page. But its not perfect
//					solution. Doesnt cover all the scenarios. So commenting it out.
//					Will have to think of alternate way to refresh plugin/package configuration page.
//					Low Priority
//						|| type == org.eclipse.emf.common.notify.Notification.REMOVE
//						|| type == org.eclipse.emf.common.notify.Notification.REMOVE_MANY ) {
//					
					
//					if (treeViewer != null)	{
//						treeViewer.refresh();
//						updateCheckStates();
//						showErrors();
//					}
				}
			}
		};
		config.eAdapters().add(configurationChangedListener);

	}

	private void reInitializeConfigFactory() {
		// the following may not be the most efficient way
		// need a fast way to update the closure
//		System.out.println("$$$ reInit closure for config add notification!");

		createConfigurationClosure();
		ConfigTreeFilter old = configFilter;
		configFilter = new ConfigTreeFilter(closure);
		if (treeViewer !=  null && old != null) {
			treeViewer.removeFilter(old);
		}
		treeViewer.addFilter(configFilter);
	}

	/**
	 * Set input for connfiguration viewer
	 * @param input
	 */
	public void setInput(Object input) {
		treeViewer.setInput(input);
		addCategoryViewer.setRoot(input);
		subCategoryViewer.setRoot(input);

		// initially expand the whole tree before updating the tree
		// this is a workaround for the content provider to build the child -
		// parent lookup map
		treeViewer.expandAll();
		treeViewer.collapseAll();
		addCategoryViewer.expandAll();
		addCategoryViewer.collapseAll();
		subCategoryViewer.expandAll();
		subCategoryViewer.collapseAll();
		
		// update the tree with the default closure selection
		updateCheckStates();
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#createFormContent(org.eclipse.ui.forms.IManagedForm)
	 */
	protected void createFormContent(IManagedForm managedForm) {
		// create form toolkit
		form = managedForm.getForm();
		form.setText(formPrefix + config.getName());
		FormToolkit toolkit = managedForm.getToolkit();

		TableWrapLayout layout = new TableWrapLayout();
		form.getBody().setLayout(layout);

		Section treeSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		createTreeContent(toolkit, treeSection);
		
		addListeners();

		// finally set the input.
		// RootContent inputNode = new RootContent(closure
		// .getConfigurationFactory().getMethodPlugins());
		setInput(LibraryService.getInstance().getCurrentMethodLibrary());
		
		initializeCategoriesViewers();

	}
	
	/**
	 * Create tree content
	 * @param toolkit
	 * @param section
	 */
	public void createTreeContent(FormToolkit toolkit, Section section) {
		section.setText(AuthoringUIResources.ConfigurationPage_ConfigContent); 
		section
				.setDescription(AuthoringUIResources.ConfigurationPage_ConfigContentDescription); 
		section.setLayoutData(new TableWrapData(TableWrapData.FILL_GRAB));

		Composite sectionClient = toolkit.createComposite(section);
		section.setClient(sectionClient);
		GridLayout gl = new GridLayout();
		sectionClient.setLayout(gl);
		gl.numColumns = 6;

		PlatformUI.getWorkbench().getHelpSystem().setHelp(
				sectionClient.getParent(),
				AuthoringUIHelpContexts.CONFIGURATION_EDITOR_ALL_CONTEXT);

		Composite buttonComposite = toolkit.createComposite(sectionClient);
		{
			GridLayout gridLayout = new GridLayout();
			gridLayout.numColumns = 6;
			buttonComposite.setLayout(gridLayout);
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.horizontalSpan = 6;
			gridData.horizontalAlignment = 3;
			buttonComposite.setLayoutData(gridData);

		}

		hideButton = toolkit.createButton(buttonComposite, "", SWT.PUSH //$NON-NLS-1$
				| GridData.HORIZONTAL_ALIGN_END);
		hideButton.setImage(AuthoringUIPlugin.getDefault().getSharedImage(
				"hideUncheckedElem.gif")); //$NON-NLS-1$
		hideButton.setToolTipText(AuthoringUIResources.ConfigurationPage_hideToolTip); 
		hideButton.setLayoutData(new GridData(GridData.END));
		hideButton.getAccessible().addAccessibleListener(new AuthoringAccessibleListener(
				AuthoringUIResources.ConfigurationPage_hideToolTip));
		
		fixWarningButton = toolkit.createButton(buttonComposite, "", SWT.PUSH); //$NON-NLS-1$
		fixWarningButton.setImage(AuthoringUIPlugin.getDefault()
				.getSharedImage("addref_co.gif")); //$NON-NLS-1$
		fixWarningButton.setToolTipText(AuthoringUIResources.ConfigurationPage_AddMissingToolTip); 
		fixWarningButton.setLayoutData(new GridData(GridData.END));
		fixWarningButton.getAccessible().addAccessibleListener(new AuthoringAccessibleListener(
				AuthoringUIResources.ConfigurationPage_AddMissingToolTip));

		// Add the closure button.
		closureButton = toolkit.createButton(buttonComposite, "", SWT.PUSH); //$NON-NLS-1$
		closureButton.setImage(AuthoringUIPlugin.getDefault().getSharedImage(
				"closure_co.gif")); //$NON-NLS-1$
		closureButton.setToolTipText(AuthoringUIResources.ConfigurationPage_MakeClosureToolTip); 
		closureButton.setLayoutData(new GridData(GridData.END));
		closureButton.getAccessible().addAccessibleListener(new AuthoringAccessibleListener(
				AuthoringUIResources.ConfigurationPage_MakeClosureToolTip));
		// closureButton.setText("");

		refreshButton = toolkit.createButton(buttonComposite, "", SWT.PUSH); //$NON-NLS-1$
		refreshButton.setImage(AuthoringUIImages.IMG_REFRESH);
		refreshButton.setToolTipText(AuthoringUIResources.refreshButton_text);
		{
			GridData gd = new GridData(GridData.END
					| GridData.HORIZONTAL_ALIGN_END);
			gd.horizontalAlignment = 3;
			gd.horizontalSpan = 1;
			refreshButton.setLayoutData(gd);
		}
		refreshButton.getAccessible().addAccessibleListener(new AuthoringAccessibleListener(
				AuthoringUIResources.refreshButton_text));

		// Create Viewer and Handle Listener for the viewer.
		createViewers(toolkit, sectionClient);

		Label elemDespLabel = toolkit
				.createLabel(sectionClient, AuthoringUIResources.ConfigurationPage_Description); 
		GridData gd1 = new GridData();
		gd1.horizontalSpan = 6;
		elemDespLabel.setLayoutData(gd1);

		elemDespContentText = toolkit.createText(sectionClient, "", SWT.NONE //$NON-NLS-1$
				| SWT.MULTI | SWT.READ_ONLY | SWT.WRAP);
		GridData gd2 = new GridData(GridData.FILL_BOTH);
		gd2.grabExcessHorizontalSpace = true;
		gd2.horizontalSpan = 6;
		gd2.heightHint = 50;

		toolkit.paintBordersFor(sectionClient);
		toolkit.paintBordersFor(buttonComposite);
		elemDespContentText.setLayoutData(gd2);
		
		// set text widget to Category viewers so they can update description field
		addCategoryViewer.setTextWidget(elemDespContentText);
		subCategoryViewer.setTextWidget(elemDespContentText);

		hideButton.setEnabled(true);
		hideButton.setVisible(true);
		refreshButton.setEnabled(true);
		refreshButton.setVisible(true);
		fixWarningButton.setEnabled(true);
		fixWarningButton.setVisible(true);
		closureButton.setEnabled(true);
		closureButton.setVisible(true);
	}
	
	private void createConfigurationClosure() {
		// refresh configuration from editor
		//
		FormEditor editor = getEditor();
		if(editor != null) {
			IEditorInput input = editor.getEditorInput();
			if(input instanceof ConfigurationEditorInput) {
				config = ((ConfigurationEditorInput)input).getConfiguration();
			}
		}
		
		closure = new ConfigurationClosure(actionMgr, config);
		closure.addListener(closureListener);
	}

	/**
	 * Initialize configuration factory
	 */
	public void initializeConfigFactory() {
		// loading the configuration closure might be slow,
		// display a progress bar
		org.eclipse.epf.library.edit.util.IRunnableWithProgress runnable = new org.eclipse.epf.library.edit.util.IRunnableWithProgress() {
			public void run(IProgressMonitor monitor)
					throws InvocationTargetException, InterruptedException {
				createConfigurationClosure();
			}

		};

		UserInteractionHelper.runWithProgress(runnable, AuthoringUIResources.ConfigurationPage_LoadingMessage); 

		AdapterFactory adapterFactory = TngAdapterFactory.INSTANCE
				.getNavigatorView_ComposedAdapterFactory();
		// AdapterFactory adapterFactory = configFactory.getAdapterFactory();
		contProvider = new ConfigPackageContentProvider(//configFactory,
				adapterFactory);
		treeViewer.setContentProvider(contProvider);
		treeViewer
				.setLabelProvider(new ConfigPackageLabelProvider(contProvider));

		configFilter = new ConfigTreeFilter(closure);
		treeViewer.addFilter(configFilter);
		
		addCategoryTreeFilter = new CategoryTreeFilter(addCategoryViewer);
		addCategoryViewer.addTreeFilter(addCategoryTreeFilter);
		subCategoryTreeFilter = new CategoryTreeFilter(subCategoryViewer);
		subCategoryViewer.addTreeFilter(subCategoryTreeFilter);
		
	}
	
	private void initializeCategoriesViewers() {
		// read from config and check the appropriate items in the CC viewers
    	List addCats = new ArrayList(config.getAddedCategory());
    	initializeCCViewer(addCategoryViewer, addCats);
    	List subCats = new ArrayList(config.getSubtractedCategory());
    	initializeCCViewer(subCategoryViewer, subCats);
    	
		addCategoryViewer.aboutToOpen();
		subCategoryViewer.aboutToOpen();
	}
	
	private void initializeCCViewer(CategoryTreeViewerWrapper viewer, List elements) {
		if (!elements.isEmpty())
			viewer.updateSelections(elements);
	}

	private void createViewers(FormToolkit toolkit, Composite sectionClient) {
		Label treeTitleLabel = toolkit.createLabel(sectionClient,
				AuthoringUIResources.ConfigurationPage_TreeTitleLabel);
		{
			GridData gd = new GridData(GridData.BEGINNING
					| GridData.VERTICAL_ALIGN_END);
			treeTitleLabel.setLayoutData(gd);
			gd.horizontalSpan = 1;
		}

		Label addCCTitleLabel = toolkit.createLabel(sectionClient,
				AuthoringUIResources.ConfigurationPage_AddCategoriesTitleLabel);
		{
			GridData gd = new GridData(GridData.BEGINNING
					| GridData.VERTICAL_ALIGN_END);
			addCCTitleLabel.setLayoutData(gd);
			gd.horizontalSpan = 1;
		}
		sectionClient.setLayoutData(new GridData(GridData.FILL_BOTH));
		GridLayout layout = new GridLayout(2, true);
		sectionClient.setLayout(layout);
		// create the library tree viewer
		treeViewer = new ConfigurationViewer(sectionClient);
		// treeViewer.getTree().setLayoutData(new GridData(GridData.FILL_BOTH));
		{
			GridData gridData = new GridData(GridData.FILL_BOTH
					| GridData.GRAB_HORIZONTAL);
			gridData.heightHint = 200;
			gridData.verticalSpan = 3;
			treeViewer.getTree().setLayoutData(gridData);
		}

		addCategoryViewer = new CategoryTreeViewerWrapper(sectionClient, 200,
				LibraryService.getInstance().getCurrentMethodLibrary(),
				new CategoryContentProvider(TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory(), config),
				new CategoryLabelProvider(TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory()), config);

		Label subCCTitleLabel = toolkit.createLabel(sectionClient,
				AuthoringUIResources.ConfigurationPage_SubCategoriesTitleLabel);
		{
			GridData gd = new GridData(GridData.BEGINNING
					| GridData.VERTICAL_ALIGN_END);
			subCCTitleLabel.setLayoutData(gd);
			gd.horizontalSpan = 1;
		}
		subCategoryViewer = new CategoryTreeViewerWrapper(sectionClient, 200,
				LibraryService.getInstance().getCurrentMethodLibrary(),
				new CategoryContentProvider(TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory(), config),
				new CategoryLabelProvider(TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory()), config);

		
		// add listener so the 2 Category viewers are in sync
		// that is, when an item is checked in one, it is unchecked in the other
		addCategoryViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				//Potentially long operation - show a busy cursor
				BusyIndicator.showWhile(subCategoryViewer.getTree().getDisplay(), new Runnable() {
					public void run() {
						if (event.getChecked())
							subCategoryViewer.selectTreeCheck(event.getElement(), false);
					}
				});
			}
		});

		subCategoryViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				//Potentially long operation - show a busy cursor
				BusyIndicator.showWhile(addCategoryViewer.getTree().getDisplay(), new Runnable() {
					public void run() {
						if (event.getChecked())
							addCategoryViewer.selectTreeCheck(event.getElement(), false);
					}
				});
			}
		});
		
		initializeConfigFactory();
	}

	private void addListeners() {
		addEditorSetFocusLiseners();		
		
		// nameText.addFocusListener(new FocusAdapter() {
		// public void focusLost(FocusEvent e) {
		// if (isTextNonEmpty(nameText)) {
		// String name = StrUtil.makeValidFileName(nameText.getText());
		// if (!name.equals(config.getName())) {
		// actionMgr.doAction(IActionManager.SET, config,
		// UmaPackage.eINSTANCE.getNamedElement_Name(),
		// name, -1);
		// form.setText(formPrefix + config.getName());
		// nameText.setText(name);
		// }
		// } else {
		// // Display a warning dialog for empty Name.
		// nameText.setText(config.getName());
		// MessageDialog.openWarning(PlatformUI.getWorkbench()
		// .getActiveWorkbenchWindow().getShell(),
		// "Enter Configuration Name",
		// "Name field cannot be empty!");
		// }
		// // config.setName(StrUtil.makeValidFileName(nameText.getText()));
		// // form.setText(formPrefix + config.getName());
		// }
		// });
		// despText.addFocusListener(new FocusAdapter() {
		// public void focusLost(FocusEvent e) {
		// // config.setBriefDescription(StrUtil.getPlainText(despText
		// // .getText()));
		// String desc1 = despText.getText();
		// if (!desc1.equals(config.getBriefDescription())) {
		// actionMgr.doAction(IActionManager.SET, config,
		// UmaPackage.eINSTANCE
		// .getMethodElement_BriefDescription(),
		// desc1, -1);
		// }
		// }
		// });
		closureButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				
				BusyIndicator.showWhile(form.getDisplay(), new Runnable() {
					public void run() {
						makeClosure();
					}
				});
			};
		});
		fixWarningButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				BusyIndicator.showWhile(form.getDisplay(), new Runnable() {
					public void run() {
						fixWarning();
					}
				});				
			};
		});
		refreshButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				BusyIndicator.showWhile(form.getDisplay(), new Runnable() {
					public void run() {
						showErrors();
					}
				});				
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		hideButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				showHideElements();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		treeViewer.addTreeListener(new ITreeViewerListener() {
			public void treeCollapsed(TreeExpansionEvent event) {
				Object element = event.getElement();
				expandedElements.remove(element);
			}

			public void treeExpanded(TreeExpansionEvent event) {
				Object element = event.getElement();
				expandedElements.add(element);

				// // this is a quick dirty fix for check state initialize issue
				// // better solution later. jxi 07/01/05
				// updateTree();
				//				
				// // restore the check state of the UI elements
				// Object obj = event.getElement();
				// boolean parentState = treeViewer.getChecked(obj);
				//				
				// ConfigPackageContentProvider cp =
				// (ConfigPackageContentProvider)treeViewer.getContentProvider();
				// Object[] children = cp.getChildren(obj);
				// if ( children != null )
				// {
				// for ( int i = 0; i < children.length; i++)
				// {
				// boolean check = false;
				// // if the folder is a UI folder, keep the check state
				// if ( children[i] instanceof MethodPlugin )
				// {
				// check =
				// config.getMethodPluginSelection().contains(children[i]);
				// }
				// else if ( children[i] instanceof MethodPackage )
				// {
				// check =
				// config.getMethodPackageSelection().contains(children[i]);
				// }
				// else
				// {
				// check = parentState;
				// }
				//						
				// boolean ret = treeViewer.setChecked(children[i], check);
				// if ( ret == false)
				// {
				// System.out.println("Can't set state");
				// }
				//
				// }
				// }
			}
		});

		// listen to the selection change of the current tree viewer
		if (selectionChangedListener == null) {
			// Create the listener
			selectionChangedListener = new ISelectionChangedListener() {
				// This just notifies those things that are affected by the
				// section.
				//
				public void selectionChanged(SelectionChangedEvent event) {
//					System.out
//							.println(AuthoringUIResources.ConfigurationPage_Selected + event.getSelection()); //$NON-NLS-1$
					setSelection(event.getSelection());
				}
			};

			treeViewer.addSelectionChangedListener(selectionChangedListener);
		}

		// add a check state change listener
		treeViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent evt) {
				
				final CheckStateChangedEvent event = evt;
				BusyIndicator.showWhile(form.getDisplay(), new Runnable() {
					public void run() {
				
				// BEGIN
				modifiedElements = new ArrayList();
				boolean checked = event.getChecked();
				updateCheckStates(event.getElement(), checked);

				/*
				 * // save the previous invalid elements List invalid =
				 * closure.getInvalidElements();
				 * 
				 * closure.setSelections(treeViewer.getCheckedElements());
				 * 
				 * if ( configFilter.isHiding() ) { treeViewer.refresh(); } else { //
				 * get the new error elements, add to the previous error
				 * elements, // and update them to update the error/warning
				 * images invalid.addAll(closure.getInvalidElements()); // also
				 * add the UI folders ConfigPackageContentProvider cp =
				 * (ConfigPackageContentProvider)treeViewer.getContentProvider();
				 * invalid.addAll(cp.getUIElements());
				 * 
				 * treeViewer.update(invalid.toArray(), null); }
				 */
				
				// need to save the config before showing error
				// since the error handling needs to check the config
				boolean saved = saveConfiguration();
				refreshCategoryViewers();
				saved = saved & saveContentCategorySelectionsToConfiguration();
				showErrors();
				
				if (!saved) {
					updateCheckStates(event.getElement(), false);

					for (int i = 0; i < modifiedElements.size(); i++) {
						treeViewer.setChecked(modifiedElements.get(i), false);
					}

				}
				
				// END
					}
				});
				
			}
		});
		
		ICheckStateListener catsCheckStateListener = new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent event) {
				
				BusyIndicator.showWhile(form.getDisplay(), new Runnable() {
					public void run() {
				
						saveContentCategorySelectionsToConfiguration();
						
						// update the closure error
						showErrors();
					}
				});
			}
		};
		
		addCategoryViewer.addCheckStateListener(catsCheckStateListener);
		subCategoryViewer.addCheckStateListener(catsCheckStateListener);

		// treeViewer.addTreeListener(new ITreeViewerListener(){
		//
		// public void treeCollapsed(TreeExpansionEvent event) {
		//				
		// }
		//
		// public void treeExpanded(TreeExpansionEvent event) {
		// //updateConfigSelection();
		//
		// // restore the check state of the UI elements
		// Object obj = event.getElement();
		// boolean parentState = treeViewer.getChecked(obj);
		//				
		// ConfigPackageContentProvider cp =
		// (ConfigPackageContentProvider)treeViewer.getContentProvider();
		// Object[] children = cp.getChildren(obj);
		// if ( children != null )
		// {
		// for ( int i = 0; i < children.length; i++)
		// {
		// boolean check = false;
		// // if the folder is a UI folder, keep the check state
		// if ( children[i] instanceof MethodPlugin )
		// {
		// check = config.getMethodPluginSelection().contains(children[i]);
		// }
		// else if ( children[i] instanceof MethodPackage )
		// {
		// check = config.getMethodPackageSelection().contains(children[i]);
		// }
		// else
		// {
		// check = parentState;
		// }
		//						
		// boolean ret = treeViewer.setChecked(children[i], check);
		// if ( ret == false)
		// {
		// System.out.println("Can't set state");
		// }
		//
		// }
		// }
		//				
		// }});

		// listen to the library changes and automatically update the
		// configuration view
		libListener = new ILibraryChangeListener() {
			public void libraryChanged(int option, Collection changedItems) {
				// for performance reason, we should not response to every
				// library change
				// only cover package and plugin changes
				if (option == ILibraryChangeListener.OPTION_DELETED
						|| option == ILibraryChangeListener.OPTION_NEWCHILD
						|| option == ILibraryChangeListener.OPTION_NEWCHILD) {
					if (changedItems != null && changedItems.size() > 0) {
						Object o = changedItems.toArray()[0];
						if (o instanceof MethodPlugin
								|| o instanceof ProcessComponent
								|| o instanceof MethodPackage
								|| o instanceof CustomCategory) {
							reInitializeConfigFactory();
							refreshViewers();
							updateCheckStates();
							showErrors();
						}
					}
				}
			}
		};

		ILibraryManager manager = LibraryService.getInstance().getCurrentLibraryManager();
		if (manager != null) {
			manager.addListener(libListener);
		}
		
		// listen to plugin presentation layout changes
		PluginUIPackageContext.INSTANCE.addListener(layoutListener);
	}

	protected void showHideElements() {
		configFilter.setHide();
		addCategoryTreeFilter.toggleHideUnchecked();
		subCategoryTreeFilter.toggleHideUnchecked();
//		initializeCCViewers();
		refreshViewers();
		updateCheckStates(); // neded to have this to update the check status
	}

	/**
	 * the UI selections contains UI folders, need to convert to the
	 * corresponding method element selections
	 * 
	 * @return Object[] the converted selections
	 */
	protected Object[] getConvertedSelections() {
		List items = new ArrayList();
		Object[] sels = treeViewer.getCheckedElements();

		ConfigPackageContentProvider cp = (ConfigPackageContentProvider) treeViewer
				.getContentProvider();
		for (int i = 0; i < sels.length; i++) {
			Object sel = cp.getUITargetElement(sels[i]);
			if (sel != null && !items.contains(sel)) {
				items.add(sel);
			}
		}

		return items.toArray();
	}

	protected void showErrors() {
		// ConfigurationClosure c = closure;
		// c.setSelections(treeViewer.getCheckedElements());

		// save the previous invalid elements
		List invalid = closure.getInvalidElements();

		// convert selections to method plugins and method packages
		//closure.setSelections(getConvertedSelections());
		closure.checkError();
		
		if (configFilter.isHiding()) {
			treeViewer.refresh();
		} else {
			// get the new error elements, add to the previous error elements,
			// and update them to update the error/warning images
			invalid.addAll(closure.getInvalidElements());

			// also add the UI folders
			ConfigPackageContentProvider cp = (ConfigPackageContentProvider) treeViewer
					.getContentProvider();
			invalid.addAll(cp.getUIElements());

			treeViewer.update(invalid.toArray(), null);
		}

		this.needUpdate = false;
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		selectionChangedListeners.add(listener);
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		selectionChangedListeners.remove(listener);
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
	 */
	public ISelection getSelection() {
		return currentSelection;
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
	 */
	public void setSelection(ISelection selection) {
		currentSelection = selection;

		for (Iterator listeners = selectionChangedListeners.iterator(); listeners
				.hasNext();) {
			ISelectionChangedListener listener = (ISelectionChangedListener) listeners
					.next();
			listener
					.selectionChanged(new SelectionChangedEvent(this, selection));
		}

		// update the description field
		IStructuredSelection sel = (IStructuredSelection) selection;
		Object o = sel.getFirstElement();
		if (o instanceof MethodElement) {
			String briefDesc = ((MethodElement) o).getBriefDescription();
			elemDespContentText.setText(briefDesc != null ? briefDesc : ""); //$NON-NLS-1$
		}

	}

	/**
	 * Make configuration closure
	 *
	 */
	protected void makeClosure() {
		closure.fixErrors();
		refreshViewers();
		updateCheckStates();
		saveConfiguration();

	}

	/**
	 * Fix all warnings 
	 */
	protected void fixWarning() {

		closure.fixProblems();
		refreshViewers();	
		updateCheckStates();
		saveConfiguration();

	}

	private void updateCheckStates(Object element, boolean checked) {

		// Object element = event.getElement();

		if (checked == true) {
			ITreeContentProvider cp = (ITreeContentProvider) treeViewer
					.getContentProvider();
			checkParent(cp, element);
			// treeViewer.setChecked(element, true);
		} else
			treeViewer.setChecked(element, false);

		selectionChildren(element, checked);

	}

	/**
	 * Select/unselect all the children for the given element
	 * @param element
	 * @param checked
	 */
	public void selectionChildren(Object element, boolean checked) {
		ITreeContentProvider cp = (ITreeContentProvider) treeViewer
				.getContentProvider();
		Object[] childs = cp.getChildren(element);
		for (int i = 0; i < childs.length; i++) {
			treeViewer.setChecked(childs[i], checked);
			selectionChildren(childs[i], checked);
		}
	}

	private void updateCheckStates() {
		treeViewer.getTree().setVisible(false);
		// treeViewer.expandAll();

		try {
			Object content = treeViewer.getInput();
			if (content instanceof MethodLibrary) {

				// set the selection since the colsure may changed with make
				// closure or fix error
				// treeViewer.setCheckedElements(closure
				// .getSelection());
				Object[] selectionList = closure.getSelection();
				ConfigPackageContentProvider cp = (ConfigPackageContentProvider) treeViewer
						.getContentProvider();

				for (int i = 0; i < selectionList.length; i++) {
					Object element = selectionList[i];

					checkParent(cp, element);
					treeViewer.setChecked(element, true);
				}

				// also set the UI folders if the corresponding package is
				// selected
				for (Iterator it = new ArrayList(cp.getUIElements()).iterator(); it
						.hasNext();) {
					Object element = it.next();
					Object o = cp.getUITargetElement(element);
					if (o != null && closure.isSelected(o)) {
						checkParent(cp, element);
						treeViewer.setChecked(element, true);
					}
				}

				// restore the expand state
				//treeViewer.setExpandedElements(expandedElements.toArray());

			}

		} finally {
			treeViewer.getTree().setVisible(true);
		}

	}

	private void checkParent(ITreeContentProvider cp, Object element) {
		if (element == null || element instanceof MethodLibrary /*
																 * || element ==
																 * currentRootNode
																 */) {
			return;
		}
		modifiedElements.add(element);
		Object parent = LibraryUtil.unwrap(cp.getParent(element));
		if (parent != null) {
			treeViewer.setChecked(parent, true);
			// configFactory.getCurrentConfiguration().add((EObject)parent,
			// false);
			checkParent(cp, parent);
		}

	}

	/**
	 * Get all parents
	 * @param list
	 * @param pkg
	 */
	public void getAllParents(List list, MethodPackage pkg) {
		MethodPackage parentPkg = ((MethodPackage) pkg).getParentPackage();
		if (parentPkg != null) {
			list.add(parentPkg);
			getAllParents(list, parentPkg);
		}
	}

	/**
	 * Check parents for the given element
	 * @param element
	 */
	public boolean setParentsChecked(Object element) {
		Widget widget = treeViewer.testFindItem(element);
		if (widget instanceof TreeItem) {
			TreeItem item = (TreeItem) widget;
			item = item.getParentItem();
			while (item != null) {
				item.setChecked(true);
				item = item.getParentItem();
			}
			return true;
		}
		return false;
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#isDirty()
	 */
	public boolean isDirty() {
		return false;
		// return isDirty;
	}

	/**
	 * Set dirty flag
	 * @param dirty
	 */
	public void setDirty(boolean dirty) {
		isDirty = dirty;
	}

	/**
	 * Content provider for configuration packages
	 */
	public class ConfigPackageContentProvider extends
			AdapterFactoryContentProvider {
		//private ConfigurationFactory configFactory;

		// private boolean showMethodModelOnly = false;

		// there is a problem in the passed in AdapterFactory
		// it can't find the correct parent from the child
		// need to build a map of child to it's parent if the child is a UI
		// folder
		// clear the map when the input is set
		Map childUIParentMap = new HashMap();

		List uiFolders = new ArrayList();

		public List getUIElements() {
			return uiFolders;
		}

		/**
		 * Create an instance
		 * @param adapterFactory
		 */
		public ConfigPackageContentProvider(//ConfigurationFactory configFactory,
				AdapterFactory adapterFactory/* , boolean showMethodModelOnly */) {
			super(adapterFactory);
			// this.showMethodModelOnly = showMethodModelOnly;
		}

		/*
		public ConfigurationFactory getConfigFactory() {
			return configFactory;
		}
		*/

		// public void setShowMethodModelOnly(boolean showOnly) {
		// showMethodModelOnly = showOnly;
		// }

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#getChildren(java.lang.Object)
		 */
		public Object[] getChildren(Object parentElement) {
			// if (parentElement instanceof RootContent) {
			// return ((RootContent) parentElement).getChildren();
			// } else {
			// if (showMethodModelOnly) {
			// return null;
			// } else {
			Object[] items = getValidItems(parentElement, super
					.getChildren(parentElement));
			return items;
			// }
			// }
		}

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#getElements(java.lang.Object)
		 */
		public Object[] getElements(Object inputElement) {
			// if (inputElement instanceof RootContent) {
			// return ((RootContent) inputElement).getChildren();
			// } else {

			Object[] items = getValidItems(inputElement, super
					.getElements(inputElement));

			// if (showMethodModelOnly && items != null) {
			// List elements = new ArrayList();
			// for (int i = 0; i < items.length; i++) {
			// if (items[i] instanceof MethodPlugin) {
			// elements.add(items[i]);
			// }
			// }
			// return elements.toArray();
			//
			// } else {
			return items;
			// }
			// }
		}

		private boolean isUIFolder(Object e) {
			return (e instanceof ContentItemProvider
					|| e instanceof ProcessesItemProvider || e instanceof MethodPackagesItemProvider ||
					e instanceof PluginUIPackagesItemProvider
			/*
			 * || e instanceof StandardCategoriesItemProvider || e instanceof
			 * DisciplineCategoriesItemProvider || e instanceof
			 * DomainsItemProvider || e instanceof WorkProductTypesItemProvider ||
			 * e instanceof RoleSetsItemProvider || e instanceof
			 * ToolsItemProvider || (e instanceof CustomCategory) &&
			 * TngUtil.isRootCustomCategory((CustomCategory)e)
			 */
			);
		}

		public Object getUITargetElement(Object e) {
			if (e instanceof CustomCategory) {
				return ((CustomCategory) e).eContainer();
			} else if (e instanceof MethodElement) {
				return e;
			} else if (e instanceof ContentItemProvider) {
				return ((ContentItemProvider) e).getParent(null);
			} else if (e instanceof ProcessesItemProvider) {
				return ((ProcessesItemProvider) e).getParent(null);
			} else if (e instanceof ItemProviderAdapter) {
				Object target = ((ItemProviderAdapter) e).getTarget();
				if (target != null && target instanceof MethodElement) {
					return target;
				}
			}

			return null;
		}

		private Object[] getValidItems(Object parent, Object[] elements) {
			if (elements == null || elements.length == 0) {
				return elements;
			}

			List pkgs = new ArrayList();
			for (int i = 0; i < elements.length; i++) {
				Object e = LibraryUtil.unwrap(elements[i]);
				boolean uiFolder = isUIFolder(e);
				if (uiFolder || e instanceof MethodPackage
						|| e instanceof MethodPlugin) {
					pkgs.add(e);

					if (uiFolder) {
						uiFolders.add(e);
					}

					// if the parent is a UI folder, map the child to the parent
					// so that we can get the parent later
					// if ( !(parent instanceof MethodElement) )
					{
						childUIParentMap.put(e, parent);
					}
				} else {
					; // System.out.println("Ignored: " + e);
				}
			}

			return pkgs.toArray();
		}

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#getParent(java.lang.Object)
		 */
		public Object getParent(Object element) {
			// if (element instanceof RootContent) {
			// return null;
			// } else
			if (childUIParentMap.containsKey(element)) {
				return childUIParentMap.get(element);
			} else {
				return super.getParent(element);
			}
		}

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#hasChildren(java.lang.Object)
		 */
		public boolean hasChildren(Object element) {
			// if (element instanceof RootContent) {
			// return true;
			// } else
			// if (showMethodModelOnly) {
			// return false;
			// } else {
			Object[] children = getChildren(element);
			return (children != null && children.length > 0);

			// return treeNodeProvider.hasChildren(element);
			// return super.hasChildren(element);
			// }
		}

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			super.inputChanged(viewer, oldInput, newInput);
			childUIParentMap.clear();
		}

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#notifyChanged(org.eclipse.emf.common.notify.Notification)
		 */
		public void notifyChanged(Notification notification) {
			super.notifyChanged(notification);

			if ((notification instanceof IViewerNotification)) {
				final IViewerNotification vnt = (IViewerNotification) notification;
				final Object element = vnt.getElement();
				final ConfigurationViewer ctrl = ((ConfigurationViewer) super.viewer);
				if (element != null && (vnt.getEventType() == Notification.ADD
						/*|| vnt.getEventType() == Notification.SET*/)) {

					SafeUpdateController.syncExec(new Runnable() {
						public void run() {
							ctrl.setChecked(element, true);
						}
					});
				}
			}
		}

		/**
		 * @see org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider#dispose()
		 */
		public void dispose() {
			super.dispose();
			uiFolders.clear();
			childUIParentMap.clear();
			//configFactory = null;
		}
	}

	/**
	 * Label provider for configuration packages
	 */
	public class ConfigPackageLabelProvider extends LabelProvider {
		ConfigPackageContentProvider contentProvider;

		//ConfigurationFactory configFactory;

		AdapterFactoryLabelProvider afProvider;

		/**
		 * Creates an instance
		 * @param contentProvider
		 */
		public ConfigPackageLabelProvider(
				ConfigPackageContentProvider contentProvider) {
			this.contentProvider = contentProvider;
			afProvider = new AdapterFactoryLabelProvider(contentProvider
					.getAdapterFactory());
			//this.configFactory = contentProvider.getConfigFactory();
		}

		private ElementDependencyError getFirstChildError(Object e) {
			ElementDependencyError error = null;
			Object[] children = contentProvider.getChildren(e);
			if (children == null || children.length == 0) {
				return null;
			}

			for (int i = 0; i < children.length; i++) {
				Object object = children[i];
				if (object instanceof ItemProviderAdapter) {
					return getFirstChildError(object);
				} else {
					error = closure.getError(object);
					if (error != null) {
						return error;
					}
				}
			}

			return error;
		}

		/**
		 * This implements {@link ILabelProvider}.getImage by forwarding it to
		 * an object that implements
		 * {@link IItemLabelProvider#getImage IItemLabelProvider.getImage}
		 */
		public Image getImage(Object object) {
			// disable for now till it works
			ConfigurationClosure config1 = closure;
			if (config1 != null) {
				// no, this will show the error mark even if the child has no
				// error, not good
				// show the error if there is child error
				// while ( object instanceof ItemProviderAdapter)
				// {
				// object = contentProvider.getParent(object);
				// }

				ElementDependencyError error = null;
				if (object instanceof ItemProviderAdapter) {
					error = getFirstChildError(object);
				} else {
					error = closure.getError(object);
				}

				if (error != null) {
					if (error.isError() || error.isChildError()) {
						return PlatformUI.getWorkbench().getSharedImages()
								.getImage(ISharedImages.IMG_OBJS_ERROR_TSK); 
					} else if (error.isWarning() || error.isChildWarning()) {
						return PlatformUI.getWorkbench().getSharedImages()
								.getImage(ISharedImages.IMG_OBJS_WARN_TSK);
					}
				}
			}

			return afProvider.getImage(object);
		}

		protected Image getImageFromObject(Object object) {
			return ExtendedImageRegistry.getInstance().getImage(object);
		}

		/**
		 * This implements {@link ILabelProvider}.getText by forwarding it to
		 * an object that implements
		 * {@link IItemLabelProvider#getText IItemLabelProvider.getText}
		 */
		public String getText(Object object) {
			if (object instanceof ProcessComponent) {
				// return
				// ((ProcessComponent)object).getProcess().getPresentationName();
				Process proc = (Process) ((ProcessComponent) object)
						.getProcess();
				if (proc != null)
					return proc.getPresentationName();
				else
					// if process is null, return processcomponent name
					return ((ProcessComponent) object).getName();
			} else
				return afProvider.getText(object);
		}

		/**
		 * @see org.eclipse.jface.viewers.LabelProvider#dispose()
		 */
		public void dispose() {
			super.dispose();

			contentProvider = null;
			afProvider = null;
			//configFactory = null;
		}
	}

	/**
	 * Filter for configuration
	 * 
	 *
	 */
	public class ConfigTreeFilter extends ViewerFilter {
		boolean hideUncheckedNodes = false;

		CheckboxTreeViewer checkTree = null;

		ConfigurationClosure closure = null;

		ConfigPackageContentProvider cp = null;

		public ConfigTreeFilter(ConfigurationClosure closure) {
			this.closure = closure;
		}

		public void setHide() {
			hideUncheckedNodes = hideUncheckedNodes == true ? false : true;
			
			if ( checkTree != null ) {
				checkTree.refresh();
			}
		}

		public boolean select(Viewer viewer, Object parentElement,
				Object element) {
			if (checkTree == null) {
				checkTree = (CheckboxTreeViewer) viewer;
				cp = (ConfigPackageContentProvider) checkTree
						.getContentProvider();
			}

			if (hideUncheckedNodes) {
				Object o = cp.getUITargetElement(element);
				return closure.isSelected(o);
			}
			return true;
		}

		public boolean isHiding() {
			return hideUncheckedNodes;
		}

		public void dispose() {
			checkTree = null;
			cp = null;
			closure = null;
		}
	}

	/**
	 * Save configuration
	 * @return
	 * 		True if configuration is save successfully, false otherwise
	 */
	public boolean saveConfiguration() {
		ConfigurationData configData = LibraryService.getInstance()
							.getConfigurationManager(config)
									.getConfigurationData();
		
		configData.setEnableUpdate(false);
		boolean ret = saveConfiguration_();
		configData.setEnableUpdate(true);

		return ret;
	}
	
	private boolean saveConfiguration_() {

		boolean oldNotify = config.eDeliver();
	    try
	    {
	    	//config.eSetDeliver(false);
	    	
			// remove the old entries
			List pkgList = config.getMethodPackageSelection();
			List plgList = config.getMethodPluginSelection();
	
			if (ConfigurationUtil.removeCollFromMethodPluginList(actionMgr, config, plgList) == false)
				return false;
			if (ConfigurationUtil.removeCollFromMethodPackageList(actionMgr, config, pkgList) == false)
				return false;
	
			// add the new entries
			pkgList = new ArrayList();
			plgList = new ArrayList();
	
			// we changed to let the ConfigurationClosure to depends on the MethodConfiguration
			// so should get the selection from the viewer checks instead of the closure
			Object[] objs = treeViewer.getCheckedElements();
			// //modifiedList.toArray();
			//Object[] objs = closure.getSelection();
	
			for (int i = 0; i < objs.length; i++) {
				if ( (objs[i] instanceof MethodPlugin) && !plgList.contains(objs[i]) ) {				
					plgList.add(objs[i]);
					
//					// add all system packages into the package selection
//					List pkgs = TngUtil.getAllSystemPackages((MethodPlugin)objs[i]);
//					for (Iterator it = pkgs.iterator(); it.hasNext();) {
//						Object pkg = it.next();
//						if (!pkgList.contains(pkg)) {
//							pkgList.add(pkg);
//						}
//					}
				}
				else if ( (objs[i] instanceof MethodPackage) && !pkgList.contains(objs[i]) ) {
					pkgList.add(objs[i]);
				}
			}
			
			if (ConfigurationUtil.addCollToMethodPluginList(actionMgr, config, plgList) == false)
				return false;
			if (ConfigurationUtil.addCollToMethodPackageList(actionMgr, config, pkgList) == false)
				return false;
	
	    	// validate before save
	    	TngUtil.validateMethodConfiguration(actionMgr, config);

			return true;
		
		} finally {
			config.eSetDeliver(oldNotify);
		}
	}
	
	/**
	 * Save configuration
	 * @return
	 * 		True if configuration is save successfully, false otherwise
	 */
	public boolean saveContentCategorySelectionsToConfiguration() {
		
    	List oldAddCats = new ArrayList(config.getAddedCategory());
    	List oldSubCats = new ArrayList(config.getSubtractedCategory());
    	
    	Set<ContentCategory> newAddCats = addCategoryViewer.getCheckedContentCategories();
    	Set<ContentCategory> newSubCats = subCategoryViewer.getCheckedContentCategories();
    	
    	oldAddCats.removeAll(newAddCats);
    	oldSubCats.removeAll(newSubCats);
    	
    	newAddCats.removeAll(config.getAddedCategory());
    	newSubCats.removeAll(config.getSubtractedCategory());
		
		if (ConfigurationUtil.removeCollFromAddedCategoryList(actionMgr, config, oldAddCats) == false)
			return false;
		
		if (!newAddCats.isEmpty()) {
			if (ConfigurationUtil.addCollToAddedCategoryList(actionMgr, config, newAddCats) == false)
				return false;
			
			Map pluginMap = MethodElementUtil.buildMap(config.getMethodPluginSelection());
			HashSet newAddedPlugins = new HashSet();
			for (Iterator<ContentCategory> it = newAddCats.iterator(); it.hasNext(); ) {
				ContentCategory cat = it.next();
				MethodPlugin plugin = UmaUtil.getMethodPlugin(cat);
				if (! pluginMap.containsKey(plugin.getGuid())) {
					if (! newAddedPlugins.contains(plugin)) {
						newAddedPlugins.add(plugin);
					}
				}
			}
			if (! newAddedPlugins.isEmpty()) {
				if (ConfigurationUtil.addCollToMethodPluginList(actionMgr, config, newAddedPlugins) == false)
					return false;
				LibraryUtil.validateMethodConfiguration(actionMgr, config);
			}
			
		}
		if (ConfigurationUtil.removeCollFromSubtractedCategoryList(actionMgr, config, oldSubCats) == false)
			return false;
		
		if (ConfigurationUtil.addCollToSubtractedCategoryList(actionMgr, config, newSubCats) == false)
			return false;

		return true;
	}


	public void dispose() {
		super.dispose();

		if (libListener != null) {
			ILibraryManager manager = LibraryService.getInstance().getCurrentLibraryManager();
			if (manager != null) {
				manager.removeListener(libListener);
			}
		}
		
		if (layoutListener != null) {
			PluginUIPackageContext.INSTANCE.removeListener(layoutListener);
		}

		if (configFilter != null) {
			configFilter.dispose();
		}

		if (closure != null) {
			closure.dispose();
			closure = null;
		}

		config.eAdapters().remove(configurationChangedListener);
	}

	/**
	 * Get configuration closure
	 */
	public ConfigurationClosure getClosure() {
		return closure;
	}
	
	private void addEditorSetFocusLiseners() {
		if (getEditor() instanceof ConfigurationEditor) {
			Listener lis = new Listener() {
				public void handleEvent (Event event) {
					if (event.data == ConfigurationPage.this && needUpdate && treeViewer != null)	{
						reInitializeConfigFactory();
						refreshViewers();
						updateCheckStates();
						showErrors();
						needUpdate = false;
					}
				}
			};
			((ConfigurationEditor) getEditor()).addToSetFocusLiseners(lis);
		}
	}
	
	private void refreshViewers() {
		treeViewer.refresh();
		refreshCategoryViewers();
	}

	private void refreshCategoryViewers() {
		addCategoryViewer.refresh();
		subCategoryViewer.refresh();
		addCategoryViewer.aboutToOpen();
		subCategoryViewer.aboutToOpen();
	}
	
	public void doQuickFix(IMarker marker) {
		if ( marker == null ) {
			return;
		}
		MethodElement element = markerHelper.getCauseMethodElement(marker);	
		if ( element == null ) {
			return;
		}
		
		if ( closure.getConfigurationManager().getConfigurationData().isElementInSubtractedCategory(element)) {
			String message = AuthoringUIResources.bind(
					AuthoringUIResources.configurationPage_QuickfixError_reason1, 
					(new String[] {LibraryUtil.getTypeName(element)}));
			AuthoringUIPlugin.getDefault().getMsgDialog()
				.displayWarning(
						AuthoringUIResources.configurationPage_quickfixError_title, 
						message);
			return;
		}
		
		Object owner = LibraryUtil.getSelectable(element);
		treeViewer.setChecked(owner, true);
		updateCheckStates(owner, true);
		saveConfiguration();
		showErrors();					
		updateCheckStates();

   }
	
	public void gotoMarker(IMarker marker) {
		// select problem in main viewer
		
		MethodElement e = ConfigurationMarkerHelper.INSTANCE.getErrorMethodElement(marker);

		if (e != null && treeViewer != null) {
			treeViewer.setSelection(LibraryUtil
					.getSelectable(e));
		}

//		msgViewListener = new ISelectionChangedListener() {
//			public void selectionChanged(SelectionChangedEvent event) {
//				IStructuredSelection sel = (IStructuredSelection) event
//						.getSelection();
//				Object[] selObjs = sel.toArray();
//				if ((selObjs != null) && (selObjs.length > 0)) {
//					EObject node = null;
//					if (selObjs[0] instanceof ElementDependencyError) {
//						node = (EObject) ((ElementDependencyError) selObjs[0])
//								.getErrorElement();
//					} else if (selObjs[0] instanceof ErrorInfo) {
//						ErrorInfo info = (ErrorInfo) selObjs[0];
//						// node = (EObject) info.getOwnerElement();
//
//						// show the cause element
//						node = (EObject) info.getCauseElement();
//
//					}
//
//					else if (selObjs[0] instanceof ElementReference) {
//						node = (EObject) ((ElementReference) selObjs[0])
//								.getRefElement();
//						if (node != null) {
//							node = node.eContainer();
//						}
//					}
//
//				}
//			}
//		};
//
//	}
		
	}
}