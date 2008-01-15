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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.common.util.AbstractTreeIterator;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIHelpContexts;
import org.eclipse.epf.authoring.ui.AuthoringUIImages;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.dialogs.ItemsFilterDialog;
import org.eclipse.epf.authoring.ui.editors.EditorChooser;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditorInput;
import org.eclipse.epf.authoring.ui.filters.ContentFilter;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichText;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichTextEditor;
import org.eclipse.epf.authoring.ui.views.ViewHelper;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.LibraryEditResources;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.command.RemoveReferencesCommand;
import org.eclipse.epf.library.edit.itemsfilter.FilterConstants;
import org.eclipse.epf.library.edit.util.Comparators;
import org.eclipse.epf.library.edit.util.Misc;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.ui.LibraryUIText;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.library.util.PluginReferenceChecker;
import org.eclipse.epf.richtext.RichTextListener;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;

import com.ibm.icu.text.DateFormat;

/**
 * Description page for method plugin
 * 
 * @author Shilpa Toraskar
 * @author Kelvin Low
 * @author Phong Nguyen Le
 * @since 1.0
 * fix for https://bugs.eclipse.org/bugs/show_bug.cgi?id=176382
 */
public class MethodPluginDescriptionPage extends BaseFormPage implements IRefreshable {

	private static final String FORM_PREFIX = LibraryUIText.TEXT_METHOD_PLUGIN
			+ ": "; //$NON-NLS-1$

	private Text ctrl_name;

	private Text ctrl_brief_desc, ctrl_r_brief_desc;

	private CheckboxTableViewer ctrl_refModel;

	private Section generalSection, refModelSection;

	private Composite generalComposite, refModelComposite;

	private boolean descExpandFlag = false;

	private MethodPlugin plugin;

	private IActionManager actionMgr;

	protected Section versionSection;

	protected Composite versionComposite;

	private Text ctrl_authors;

	private Text ctrl_change_date;

	private Text ctrl_change_desc;

	private Text ctrl_version;

	protected boolean versionSectionOn = true;

	protected boolean anyAttributeModified = false;

	protected static final int VERSION_SECTION_ID = 3;

	protected IMethodRichTextEditor ctrl_version_expanded;

	protected ImageHyperlink expandVersionLink;

	protected Label expandVersionLabel;

	protected Composite expandedVersionComposite;

	protected ModifyListener modelModifyListener;

	protected ModifyListener contentModifyListener;

	protected static final int GENERAL_SECTION_ID = 1;

	private IMethodRichText activeControl;

	protected Label label_copyright;

	protected TableViewer copyright_viewer;

	protected Table ctrl_copyright;

	protected IStructuredContentProvider copyrightContentProvider;

	private ILabelProvider labelProviderBase = new AdapterFactoryLabelProvider(
			TngAdapterFactory.INSTANCE
					.getNavigatorView_ComposedAdapterFactory());;

	private Button copyright_button;

	private Button copyright_button_deselect;

	private Button ctrl_changeable;

	public boolean notificationEnabled = true;

	protected Adapter userChangeableAdapter;

	private ModifyListener nameModifyListener;

	/**
	 * Creates a new instance.
	 */
	public MethodPluginDescriptionPage(FormEditor editor) {
		super(editor, AuthoringUIText.DESCRIPTION_PAGE_TITLE,
				AuthoringUIText.DESCRIPTION_PAGE_TITLE);

		actionMgr = ((MethodElementEditor) editor).getActionManager();
		userChangeableAdapter = new UserChangeableAdapter();
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.BaseFormPage#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		MethodElementEditorInput methodElementInput = (MethodElementEditorInput) input;
		Object obj = methodElementInput.getMethodElement();
		plugin = (MethodPlugin) obj;
		if (userChangeableAdapter != null) {
			plugin.eAdapters().add(userChangeableAdapter);
		}
	}

	/**
	 * @see org.eclipse.ui.forms.editor.createFormContent(IManagedForm)
	 */
	protected void createFormContent(IManagedForm managedForm) {
		super.createFormContent(managedForm);
		createEditorContent(toolkit);
		createReferenceContent(toolkit);
		setContextHelp();
		loadData();
		addListeners();
	}

	private void setContextHelp() {
		if (generalComposite != null) {
			PlatformUI
					.getWorkbench()
					.getHelpSystem()
					.setHelp(
							generalComposite.getParent().getParent(),
							AuthoringUIHelpContexts.PLUGIN_EDITOR_DESCRIPTION_ALL_CONTEXT);
		}
	}

	/**
	 * Creates the editor page content.
	 * 
	 * @param toolkit
	 *            The form toolkit.
	 */
	protected void createEditorContent(FormToolkit toolkit) {
		form.setText(FORM_PREFIX + plugin.getName());

		// Create the General section.
		generalSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);

		TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
		generalSection.setLayoutData(td);
		generalSection.setText(AuthoringUIText.GENERAL_INFO_SECTION_NAME);
		generalSection.setDescription(MessageFormat.format(
				AuthoringUIText.GENERAL_INFO_SECTION_DESC,
				new String[] { LibraryUIText.getUITextLower(methodElement) }));
		generalSection.setLayout(new GridLayout());

		generalComposite = toolkit.createComposite(generalSection);
		generalComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		generalComposite.setLayout(new GridLayout(3, false));
		generalSection.setClient(generalComposite);

		// name
		ctrl_name = createTextEditWithLabel(toolkit, generalComposite,
				AuthoringUIText.NAME_TEXT);

		// brief desc
		ctrl_brief_desc = createTextEditWithLabel2(toolkit, generalComposite,
				AuthoringUIText.BRIEF_DESCRIPTION_TEXT);

		if (versionSectionOn) {
			createVersionSection(toolkit);
			createVersionSectionContent();
		}

		toolkit.paintBordersFor(generalComposite);

		// set focus on the name attribute
		Display display = form.getBody().getDisplay();
		if (!(display == null || display.isDisposed())) {
			display.asyncExec(new Runnable() {
				public void run() {
					ctrl_name.setFocus();
				}
			});
		}
	}

	private void createReferenceContent(FormToolkit toolkit) {
		// Ref Model Section
		refModelSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td1 = new TableWrapData(TableWrapData.FILL_GRAB);
		refModelSection.setLayoutData(td1);
		refModelSection
				.setText(AuthoringUIText.REFERENCED_PLUGINS_SECTION_NAME);
		refModelSection
				.setDescription(AuthoringUIText.REFERENCED_PLUGINS_SECTION_DESC);
		refModelSection.setLayout(new GridLayout());

		refModelComposite = toolkit.createComposite(refModelSection);
		refModelComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		refModelComposite.setLayout(new GridLayout(2, false));
		refModelSection.setClient(refModelComposite);

		Table ctrl_table = toolkit.createTable(refModelComposite, SWT.CHECK);
		{
			GridData gridData = new GridData(GridData.BEGINNING
					| GridData.FILL_BOTH);
			gridData.heightHint = 100;
			ctrl_table.setLayoutData(gridData);
		}

		ctrl_refModel = new CheckboxTableViewer(ctrl_table);
		ILabelProvider labelProvider = new LabelProvider() {
			public String getText(Object element) {
				MethodPlugin plugin = (MethodPlugin) element;
				return plugin.getName();
			}
		};
		ctrl_refModel.setLabelProvider(labelProvider);

		Label l_r_brief_desc = toolkit.createLabel(refModelComposite,
				AuthoringUIText.BRIEF_DESCRIPTION_TEXT);
		{
			GridData gridData = new GridData(GridData.BEGINNING);
			gridData.horizontalSpan = 3;
			l_r_brief_desc.setLayoutData(gridData);
		}

		ctrl_r_brief_desc = toolkit.createText(refModelComposite,
				"", SWT.MULTI | SWT.WRAP | SWT.V_SCROLL | SWT.READ_ONLY); //$NON-NLS-1$
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL
					| GridData.GRAB_HORIZONTAL);
			gridData.heightHint = 80;
			ctrl_r_brief_desc.setLayoutData(gridData);
		}

		toolkit.paintBordersFor(refModelComposite);
	}

	private void setCheckboxForCurrentBase(List currentBaseList) {
		ctrl_refModel.setAllChecked(false);
		for (int i = 0; i < currentBaseList.size(); i++) {
			MethodPlugin model = (MethodPlugin) currentBaseList.get(i);
			ctrl_refModel.setChecked(model, true);
		}
	}

	/**
	 * Add listeners
	 * 
	 */
	private void addListeners() {
		MethodElementEditor editor = (MethodElementEditor) getEditor();

		form.addListener(SWT.Activate, new Listener() {
			public void handleEvent(Event e) {
				// Clear the old items and add the newly allowable items.
				ctrl_refModel.getTable().clearAll();
				ctrl_refModel.refresh();

				List allowableList = PluginReferenceChecker
						.getApplicableBasePlugins(plugin);
				Collections.<Object>sort(allowableList, Comparators.PLUGINPACKAGE_COMPARATOR);
				ctrl_refModel.add(allowableList.toArray());

				List currentBaseList = plugin.getBases();
				setCheckboxForCurrentBase(currentBaseList);

				if (!plugin.getUserChangeable().booleanValue()) {
					enableControls(false);
				} else {
					enableControls(true);
				}
				copyright_viewer.refresh();
			}
		});

		final ModifyListener modifyListener = editor
				.createModifyListener(plugin);

		nameModifyListener = editor.createModifyListener(plugin, true);
		ctrl_name.addModifyListener(nameModifyListener);
		ctrl_name.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				String oldContent = plugin.getName();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				if (ctrl_name.getText().equals(plugin.getName())) {
					return;
				}

				// Check invalid characters first.
				
				// 178462
				String msg = null;
				String name = ctrl_name.getText().trim();
				if (name != null) {
					if (oldContent.indexOf("&") < 0 && name.indexOf("&") > -1) { //$NON-NLS-1$ //$NON-NLS-2$
						msg = NLS
								.bind(
										LibraryEditResources.invalidElementNameError4_msg,
										name);
					} else {
						msg = LibraryUtil.checkPluginName(null, name);
					}
				}
				
				String validName = StrUtil.makeValidFileName(ctrl_name
						.getText());
				if (msg == null) {
					// Check duplicate plug-in name.
					msg = LibraryUtil.checkPluginName(plugin, validName);
				}
				if (msg == null) {
					if (!validName.equals(plugin.getName())) {
						Shell shell = getSite().getShell();
						msg = AuthoringUIResources.bind(AuthoringUIResources.methodPluginDescriptionPage_confirmRename, (new Object[] { plugin.getName(), ctrl_name.getText() })); 
						String title = AuthoringUIResources.methodPluginDescriptionPage_confirmRename_title; 
						if (!MessageDialog.openConfirm(shell, title, msg)) {
							ctrl_name.setText(plugin.getName());
							return;
						}

						e.doit = true;
						EditorChooser.getInstance().closeMethodEditorsForPluginElements(plugin);
						ctrl_name.setText(validName);
						boolean status = actionMgr.doAction(IActionManager.SET,
								plugin, UmaPackage.eINSTANCE
										.getNamedElement_Name(), validName, -1);

						if (!status) {
							return;
						}
						form.setText(FORM_PREFIX + plugin.getName());
						updateChangeDate();

						// adjust plugin location and save the editor
						//
						BusyIndicator.showWhile(getSite().getShell()
								.getDisplay(), new Runnable() {
							public void run() {
								MethodElementEditor editor = (MethodElementEditor) getEditor();
								editor.doSave(new NullProgressMonitor());
								ILibraryPersister.FailSafeMethodLibraryPersister persister = editor
										.getPersister();
								try {
									persister
											.adjustLocation(plugin.eResource());
									persister.commit();
								} catch (RuntimeException e) {
									AuthoringUIPlugin.getDefault().getLogger()
											.logError(e);
									try {
										persister.rollback();
									} catch (Exception ex) {
										AuthoringUIPlugin.getDefault()
												.getLogger().logError(ex);
										ViewHelper
												.reloadCurrentLibaryOnRollbackError(getSite()
														.getShell());
										return;
									}
									AuthoringUIPlugin
											.getDefault()
											.getMsgDialog()
											.displayWarning(
													getSite().getShell()
															.getText(),
													AuthoringUIResources.methodPluginDescriptionPage_cannotRenamePluginFolder
													, e.getMessage(), e);
								}
							}
						});
					}
				} else {
					ctrl_name.setText(plugin.getName());
					Shell shell = getSite().getShell();
					AuthoringUIPlugin.getDefault().getMsgDialog().displayError(
							shell.getText(), msg);
					e.doit = false;
					ctrl_name.getDisplay().asyncExec(new Runnable() {
						public void run() {
							ctrl_name.setFocus();
							ctrl_name.selectAll();
						}
					});
				}
			}
		});
		ctrl_name.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getNamedElement_Name());
			}
		});

		ctrl_brief_desc.addModifyListener(modifyListener);
		ctrl_brief_desc.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_BriefDescription());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = plugin.getBriefDescription();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newContent = ctrl_brief_desc.getText();
				if (!newContent.equals(oldContent)) {
					boolean status = actionMgr.doAction(IActionManager.SET,
							plugin, UmaPackage.eINSTANCE
									.getMethodElement_BriefDescription(),
							newContent, -1);
					if (status) {
						ctrl_brief_desc.setText(newContent);
					}
				}
			}
		});

		ctrl_refModel
				.addSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						StructuredSelection selectedList = (StructuredSelection) event
								.getSelection();
						MethodPlugin selectedObj = (MethodPlugin) selectedList
								.getFirstElement();
						if (selectedObj == null)
							return;
						ctrl_r_brief_desc.setText(selectedObj
								.getBriefDescription());
					}

				});

		ctrl_refModel.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent event) {
				Object obj = event.getElement();

				ctrl_r_brief_desc.setText(((MethodPlugin) obj)
						.getBriefDescription());

				if (TngUtil.isLocked(plugin)) {
					ctrl_refModel.setChecked(obj, !event.getChecked());
					return;
				}

				if (event.getChecked()) {
					// TODO: Change this to be not un-doable due to the circular
					// dependency check.
					actionMgr.doAction(IActionManager.ADD, plugin,
							UmaPackage.eINSTANCE.getMethodPlugin_Bases(),
							(MethodPlugin) obj, -1);
				} else {
					final MethodPlugin base = (MethodPlugin) obj;
					
					if(removeAllReferences(base)){
						ctrl_refModel.setChecked(base, false);
					}else{
						Display.getCurrent().asyncExec(new Runnable() {
							public void run() {
								ctrl_refModel.setChecked(base, true);
							}
						});
						return;
					}
					// change this to be not un-doable due to the circular
					// dependency check
					// plugin.getBases().remove(obj);
					actionMgr.doAction(IActionManager.REMOVE, plugin,
							UmaPackage.eINSTANCE.getMethodPlugin_Bases(),
							(MethodPlugin) obj, -1);
				}

				// double check circular dependency, not necessary here
				PluginReferenceChecker.hasCircularConflictWithPlugin(plugin);

				updateChangeDate();
			}

		});

		if (versionSectionOn) {
			addVersionSectionListeners();
		}
	}

	protected void enableControls(boolean editable) {
		ctrl_name.setEditable(editable);
		ctrl_authors.setEditable(editable);
		ctrl_brief_desc.setEditable(editable);
		ctrl_version.setEditable(editable);
		ctrl_change_desc.setEditable(editable);
		copyright_button.setEnabled(editable);
		copyright_button_deselect.setEnabled(editable);
		ctrl_r_brief_desc.setEditable(false);
	}

	/**
	 * Loads initial data from model
	 */
	private void loadData() {
		String name = plugin.getName();
		String desc = plugin.getBriefDescription();
		ctrl_name.setText(name == null ? "" : name); //$NON-NLS-1$
		ctrl_brief_desc.setText(desc == null ? "" : desc); //$NON-NLS-1$

		if (versionSectionOn) {
			loadVersionSectionData();
		}
	}

	/**
	 * Check to see whether version section is visible or not
	 * @return Returns the versionSectionOn.
	 */
	public boolean isVersionSectionOn() {
		return versionSectionOn;
	}

	/**
	 * Set version section to be visible or not
	 * @param versionSectionOn
	 *            The versionSectionOn to set.
	 */
	public void setVersionSectionOn(boolean versionSectionOn) {
		this.versionSectionOn = versionSectionOn;
	}

	private void createVersionSection(FormToolkit toolkit) {
		versionSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
		versionSection.setLayoutData(td);
		versionSection.setText(AuthoringUIText.VERSION_INFO_SECTION_NAME);
		versionSection.setDescription(MessageFormat.format(
				AuthoringUIText.VERSION_INFO_SECTION_DESC,
				new String[] { LibraryUIText.getUITextLower(methodElement) }));
		versionSection.setLayout(new GridLayout());

		versionComposite = toolkit.createComposite(versionSection);
		versionComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		versionComposite.setLayout(new GridLayout(5, false));
		versionSection.setClient(versionComposite);
	}

	/**
	 * Create the Version section content.
	 */
	private void createVersionSectionContent() {
		String fillText = (plugin.getVersion() == null ? "" : plugin.getVersion()); //$NON-NLS-1$
		ctrl_version = createTextEditWithLabel4(toolkit, versionComposite,
				AuthoringUIText.VERSION_TEXT, SWT.DEFAULT, SWT.SINGLE, fillText);
		
		fillText = plugin.getChangeDate() == null ? "" : //$NON-NLS-1$
			DateFormat.getDateInstance(DateFormat.FULL).format(
					plugin.getChangeDate());

		ctrl_change_date = createTextEditWithLabel4(toolkit, versionComposite,
				AuthoringUIText.CHANGE_DATE_TEXT, SWT.DEFAULT, SWT.SINGLE, fillText);
		ctrl_change_date.setEditable(false);
		
		ctrl_change_desc = createTextEditWithLabel5(toolkit, versionComposite,
				AuthoringUIText.CHANGE_DESCRIPTION_TEXT, 40, SWT.MULTI);
	
		ctrl_authors = createTextEditWithLabel5(toolkit, versionComposite,
				AuthoringUIText.AUTHORS_TEXT, 40, SWT.MULTI);
		
		label_copyright = createLabel(toolkit, versionComposite,
				AuthoringUIText.COPYRIGHT_TEXT, 2);
		ctrl_copyright = createTable(toolkit, versionComposite, SWT.SINGLE
				| SWT.READ_ONLY, GridData.FILL_HORIZONTAL | GridData.BEGINNING | GridData.FILL_VERTICAL,
				5, 400, 1, 2);
		copyright_viewer = new TableViewer(ctrl_copyright);
		initContentProviderCopyright();
		copyright_viewer.setLabelProvider(labelProviderBase);
		copyright_viewer.setInput(plugin);
		
		
		Composite buttonpane = createComposite(toolkit, versionComposite,
				GridData.HORIZONTAL_ALIGN_END, 1, 1, 1);
		{
			GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_END);
			buttonpane.setLayoutData(gridData);
		}
		copyright_button = toolkit.createButton(buttonpane,
				AuthoringUIText.SELECT_BUTTON_TEXT, SWT.SIMPLE);
		{
			GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_END);
			gridData.widthHint = DescriptionFormPage.BUTTON_WIDTH;
			copyright_button.setLayoutData(gridData);
		}
		copyright_button_deselect = toolkit.createButton(buttonpane,
				AuthoringUIText.DESELECT_BUTTON_TEXT, SWT.SIMPLE);
		{
			GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_END);
			gridData.widthHint = DescriptionFormPage.BUTTON_WIDTH;
			copyright_button_deselect.setLayoutData(gridData);
		}		

		ctrl_changeable = toolkit
				.createButton(
						versionComposite,
						AuthoringUIResources.methodPluginDescriptionPage_lockPluginLabel, SWT.CHECK); 

		// Create the expanded composite.
		expandedVersionComposite = toolkit.createComposite(versionSection,
				SWT.NONE);
		expandedVersionComposite.setLayoutData(new GridData(
				GridData.FILL_HORIZONTAL));
		expandedVersionComposite.setLayout(new GridLayout(2, false));
		expandedVersionComposite.setVisible(false);

		// Add the expand/collapse hyperlink image.
		expandVersionLink = toolkit.createImageHyperlink(
				expandedVersionComposite, SWT.NONE);
		expandVersionLink.setImage(AuthoringUIImages.IMG_EXPANDED);
		expandVersionLink.setUnderlined(false);
		expandVersionLink.setToolTipText(AuthoringUIResources.closeRTE); 
		expandVersionLink.addHyperlinkListener(new HyperlinkAdapter() {
			public void linkActivated(HyperlinkEvent e) {
				toggle(e, VERSION_SECTION_ID);
			}
		});

		// Add the expand/collapse hyperlink text.
		expandVersionLabel = createDecoratedLabel(toolkit, expandedVersionComposite, ""); //$NON-NLS-1$
		toolkit.paintBordersFor(expandedVersionComposite);
		toolkit.paintBordersFor(versionComposite);
	}

	private void initContentProviderCopyright() {
		copyrightContentProvider = new AdapterFactoryContentProvider(
				TngAdapterFactory.INSTANCE
						.getNavigatorView_ComposedAdapterFactory()) {
			public Object[] getElements(Object object) {
				List list = new ArrayList();
				if (plugin.getCopyrightStatement() != null) {
					list.add(plugin.getCopyrightStatement());
				}
				return list.toArray();
			}
		};
		copyright_viewer.setContentProvider(copyrightContentProvider);
	}

	protected void addVersionSectionListeners() {

		final MethodElementEditor editor = (MethodElementEditor) getEditor();

		modelModifyListener = editor.createModifyListener(plugin);
		contentModifyListener = editor.createModifyListener(plugin);

		ctrl_version.addModifyListener(contentModifyListener);
		ctrl_version.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodUnit_Version());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = plugin.getVersion();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newContent = StrUtil
						.getPlainText(ctrl_version.getText());
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, plugin,
							UmaPackage.eINSTANCE.getMethodUnit_Version(),
							newContent, -1);
					updateChangeDate();
				}
			}
		});

		ctrl_authors.addModifyListener(contentModifyListener);
		ctrl_authors.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodUnit_Authors());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = plugin.getAuthors();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newContent = StrUtil
						.getPlainText(ctrl_authors.getText());
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, plugin,
							UmaPackage.eINSTANCE.getMethodUnit_Authors(),
							newContent, -1);
					updateChangeDate();
				}
			}
		});

		copyright_button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				IFilter filter = new ContentFilter() {
					protected boolean childAccept(Object obj) {
						if (obj instanceof Guidance) {
							return (obj instanceof SupportingMaterial);
						}
						return false;
					}
				};
				List alreadyExsting = new ArrayList();
				if (ctrl_copyright.getItemCount() > 0) {
					TableItem item = ctrl_copyright.getItems()[0];
					if (item.getData() != null)
						alreadyExsting.add(item.getData());
				}
				ItemsFilterDialog fd = new ItemsFilterDialog(PlatformUI
						.getWorkbench().getActiveWorkbenchWindow().getShell(),
						filter, plugin, FilterConstants.SUPPORTING_MATERIALS,
						alreadyExsting);
				fd.setViewerSelectionSingle(true);
				fd.setBlockOnOpen(true);
				fd.setTitle(FilterConstants.SUPPORTING_MATERIALS);
				fd.open();
				fd.getSelectedItems();
				if (fd.getSelectedItems().size() > 0) {
					editor.getActionManager().doAction(
							IActionManager.SET,
							plugin,
							UmaPackage.eINSTANCE
									.getMethodUnit_CopyrightStatement(),
							(SupportingMaterial) fd.getSelectedItems().get(0),
							-1);
				}
				copyright_viewer.refresh();

			}
		});

		ctrl_change_desc.addModifyListener(contentModifyListener);
		ctrl_change_desc.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodUnit_ChangeDescription());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = plugin.getChangeDescription();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newContent = ctrl_change_desc.getText();

				newContent = newContent.replace(StrUtil.LINE_FEED, AuthoringUIResources.ChangeHistoryDialog_delimiter);

				if (!newContent.equals(oldContent)) {
					boolean success = actionMgr.doAction(
							IActionManager.SET, plugin,
							UmaPackage.eINSTANCE
									.getMethodUnit_ChangeDescription(),
							newContent, -1);
					if (success) {
						updateChangeDate();
					}
				}
			}

		});

		copyright_button_deselect.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				actionMgr
						.doAction(IActionManager.SET, plugin,
								UmaPackage.eINSTANCE
										.getMethodUnit_CopyrightStatement(),
								null, -1);
				copyright_viewer.refresh();
			}
		});

		ctrl_changeable.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				notificationEnabled = true;

				
				IStatus status = TngUtil.checkEdit(plugin, getSite().getShell());
				if (status.isOK()) {
					
					String message = AuthoringUIResources
							.bind(
									AuthoringUIResources.methodPluginDescriptionPage_lockPlugin_message,
									plugin.getName());
					Shell shell = getSite().getShell();
					if (AuthoringUIPlugin.getDefault().getMsgDialog()
							.displayConfirmation(shell.getText(), message)) {
						// close editors on any change in ctrl_changeable
						// if (ctrl_changeable.getSelection()) {
						EditorChooser.getInstance().closeMethodEditorsForPluginElements(plugin);
						boolean ret = actionMgr.doAction(IActionManager.SET,
								plugin, UmaPackage.eINSTANCE
										.getMethodPlugin_UserChangeable(),
								new Boolean(!ctrl_changeable.getSelection()),
								-1);
						// in case of readonly file, roll back changes.
						if (!ret) {
							ctrl_changeable.setSelection(!ctrl_changeable
									.getSelection());
							return;
						}

						// }
						enableControls(!ctrl_changeable.getSelection());
					} else {
						// actionMgr.undo();
						ctrl_changeable.setSelection(!ctrl_changeable
								.getSelection());
						enableControls(!ctrl_changeable.getSelection());
						// return;
					}
				} else {
					AuthoringUIPlugin.getDefault().getMsgDialog().displayError(
							AuthoringUIResources.editDialog_title,
							AuthoringUIResources.editDialog_msgCannotEdit,
							status);
					return;
				}
				copyright_viewer.refresh();
				
				// refresh editor title image. 
				((MethodElementEditor)getEditor()).refreshTitleImage();
			}

			
		});
	}

	/*
	 * update the change date if any attribute is modified.
	 */
	protected void updateChangeDate() {

		Date changeDate = plugin.getChangeDate();
		DateFormat df = DateFormat.getDateInstance(DateFormat.FULL);

		String oldContent = ""; //$NON-NLS-1$
		if (changeDate != null) {
			oldContent = df.format(changeDate);
		}

		Date currentDate = new Date();
		String newContent = df.format(currentDate);
		if (!newContent.equals(oldContent)) {
			actionMgr.doAction(IActionManager.SET, plugin, UmaPackage.eINSTANCE
					.getMethodUnit_ChangeDate(), currentDate, -1);
			ctrl_change_date.setText(newContent);
		}
	}

	protected void loadVersionSectionData() {
		ctrl_version
				.setText(plugin.getVersion() == null ? "" : plugin.getVersion()); //$NON-NLS-1$
		ctrl_authors
				.setText(plugin.getAuthors() == null ? "" : plugin.getAuthors()); //$NON-NLS-1$
		String changeDesc = plugin.getChangeDescription() == null ? "" : plugin.getChangeDescription(); //$NON-NLS-1$
		changeDesc = changeDesc.replace(AuthoringUIResources.ChangeHistoryDialog_delimiter, StrUtil.LINE_FEED);
		ctrl_change_desc.setText(changeDesc);
		ctrl_change_date.setText(plugin.getChangeDate() == null ? "" : //$NON-NLS-1$
				DateFormat.getDateInstance(DateFormat.FULL).format(
						plugin.getChangeDate())); 
		ctrl_changeable
				.setSelection(!plugin.getUserChangeable().booleanValue());
	}

	/**
	 * Toggle Description control to expand and control state
	 * 
	 */
	protected void toggle(HyperlinkEvent e, int id) {
		// TODO: We should combine these methods into one. One way to do it,
		// dispoing
		// ctrl_expanded every time it collapses and creating it when we expand.
		// At present, there is no method to dispose
		if (id == GENERAL_SECTION_ID) {
			toggle(e);
		}
		if (id == VERSION_SECTION_ID) {
			toggleVersionSection(e);
		}
	}

	private void toggleVersionSection(HyperlinkEvent e) {
		if (ctrl_version_expanded == null) {
			ctrl_version_expanded = createRichTextEditor(toolkit,
					expandedVersionComposite, SWT.MULTI | SWT.WRAP
							| SWT.V_SCROLL, GridData.FILL_BOTH, getRichTextEditorHeight(), getRichTextEditorWidth(), 2,
							expandVersionLabel); 
			ctrl_version_expanded.addModifyListener(contentModifyListener);
		}

		if (descExpandFlag) {
			versionComposite.setVisible(true);
			expandedVersionComposite.setVisible(false);
			versionSection.setClient(versionComposite);
			refModelSection.setExpanded(true);
			generalSection.setExpanded(true);
			IMethodRichText richText = getActiveRichTextControl();
			richText.setText(ctrl_version_expanded.getText());
			for (Iterator i = richText.getListeners(); i.hasNext();) {
				RichTextListener listener = (RichTextListener) i.next();
				ctrl_version_expanded.removeListener(listener.getEventType(),
						listener.getListener());
			}
			if (ctrl_version_expanded.getModified()) {
				((MethodElementEditor) getEditor())
						.saveModifiedRichText(ctrl_version_expanded);
			}
			richText.setFocus();
		} else {
			versionComposite.setVisible(false);
			expandedVersionComposite.setVisible(true);
			versionSection.setClient(expandedVersionComposite);
			refModelSection.setExpanded(false);
			generalSection.setExpanded(false);
			expandVersionLabel.setText((String) ((ImageHyperlink) e.getSource())
							.getData("Title")); //$NON-NLS-1$    		
			IMethodRichText richText = (IMethodRichText) e.getHref();
			ctrl_version_expanded.setText(richText.getText());
			ctrl_version_expanded.setModalObject(richText.getModalObject());
			ctrl_version_expanded.setModalObjectFeature(richText
					.getModalObjectFeature());
			for (Iterator i = richText.getListeners(); i.hasNext();) {
				RichTextListener listener = (RichTextListener) i.next();
				ctrl_version_expanded.addListener(listener.getEventType(),
						listener.getListener());
			}
			ctrl_version_expanded.setFocus();
			setActiveRichTextControl(richText);
		}

		versionSection.layout(true);
		descExpandFlag = !descExpandFlag;
	}

	/**
	 * Set active rich text control
	 */
	private void setActiveRichTextControl(IMethodRichText ctrl) {
		activeControl = ctrl;
	}

	/**
	 * Get Active Rich text control.
	 */
	private IMethodRichText getActiveRichTextControl() {
		return activeControl;
	}

	protected class UserChangeableAdapter extends AdapterImpl {
		public void notifyChanged(Notification msg) {
			switch (msg.getFeatureID(MethodPlugin.class)) {
			case UmaPackage.METHOD_PLUGIN__USER_CHANGEABLE:
				Boolean b = (Boolean) msg.getNewValue();
				setUserChangeable(b.booleanValue());
				return;
			}
		}
	}

	public void setUserChangeable(boolean userChangeable) {
		if (!notificationEnabled)
			return;
		notificationEnabled = false;
		plugin.setUserChangeable(Boolean.valueOf(userChangeable));
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#dispose()
	 */
	public void dispose() {
		plugin.eAdapters().remove(userChangeableAdapter);
		super.dispose();
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.IRefreshable#refreshName(java.lang.String)
	 */
	public void refreshName(String newName) {
		if (newName != null) {
			if ((ctrl_name != null) && !(ctrl_name.isDisposed())) {
				ctrl_name.removeModifyListener(nameModifyListener);
				ctrl_name.setText(newName);
				ctrl_name.addModifyListener(nameModifyListener);
				form.setText(FORM_PREFIX + plugin.getName());
			}
		}
	}
	
	/**
	 * Checks if the given MethodPlugin <code>base</code> is one in the given
	 * plugins collection or base of any plugin in the collection.
	 * 
	 * @param base
	 * @param plugins
	 * @return
	 */
	private static boolean isOneOrBaseOf(MethodPlugin base, Collection<MethodPlugin> plugins) {
		for (MethodPlugin plugin : plugins) {
			if(base == plugin || Misc.isBaseOf(base, plugin)) {
				return true;
			}
		}
		return false;
	}

	private boolean removeAllReferences(MethodPlugin unCheckedPlugin) {
		ArrayList<MethodPlugin> removedBases = new ArrayList<MethodPlugin>();
		removedBases.add(unCheckedPlugin);
		for (Iterator<MethodPlugin> iter = new AbstractTreeIterator<MethodPlugin>(unCheckedPlugin, false) {
		
			private static final long serialVersionUID = 1L;

			@Override
			protected Iterator<? extends MethodPlugin> getChildren(Object object) {
				if(object instanceof MethodPlugin) {
					return ((MethodPlugin)object).getBases().iterator();
				}
				return Collections.EMPTY_LIST.iterator();
			}
		
		}; iter.hasNext();) {
			MethodPlugin base = iter.next();
			ArrayList<MethodPlugin> plugins = new ArrayList<MethodPlugin>(plugin.getBases());
			plugins.remove(unCheckedPlugin);
			if(!isOneOrBaseOf(base, plugins)) {
				removedBases.add(base);
			}
		}
		
		ArrayList<MethodPlugin> affectedPlugins = new ArrayList<MethodPlugin>();
		
		// get all plug-ins in library that extend this plug-in
		//
		List<?> plugins = LibraryService.getInstance().getCurrentMethodLibrary()
				.getMethodPlugins();
		for (Iterator<?> iterator = plugins.iterator(); iterator.hasNext();) {
			MethodPlugin mp = (MethodPlugin) iterator.next();
			if(mp != plugin && Misc.isBaseOf(plugin, mp)) {				
				affectedPlugins.add(mp);
			}
		}
		
		ArrayList<RemoveReferencesCommand> commands = new ArrayList<RemoveReferencesCommand>();
		for (MethodPlugin base : removedBases) {			
			if(UmaUtil.hasReference(plugin, base)) {
				commands.add(new RemoveReferencesCommand(plugin, base));
			}
			for (MethodPlugin mp : affectedPlugins) {
				ArrayList<MethodPlugin> bases = new ArrayList<MethodPlugin>(mp.getBases());
				bases.remove(base);
				if(!isOneOrBaseOf(base, bases) && UmaUtil.hasReference(mp, base)) {
					commands.add(new RemoveReferencesCommand(mp, base));
				}
			}
		}

		if (!commands.isEmpty()) {
			String message = AuthoringUIResources
					.bind(AuthoringUIResources.methodPluginDescriptionRemoveRefConfirm_message,
							plugin.getName());
			Shell shell = getSite().getShell();
			if (AuthoringUIPlugin.getDefault().getMsgDialog()
					.displayConfirmation(shell.getText(), message)) {
				int i = 0;
				try {
					for (RemoveReferencesCommand cmd : commands) {
						actionMgr.execute(cmd);
						i++;
					}
					return true;
				}
				catch(Exception e) {
					AuthoringUIPlugin.getDefault().getLogger().logError(e);
					// undo the executed commands
					for (; i > 0; i--) {
						actionMgr.undo();
					}
					return false;
				}
			} else {
				return false;
			}
		}
		return true;
	}
}