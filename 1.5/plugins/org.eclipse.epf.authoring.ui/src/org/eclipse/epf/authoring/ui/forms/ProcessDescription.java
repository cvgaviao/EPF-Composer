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
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.edit.provider.IStructuredItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIHelpContexts;
import org.eclipse.epf.authoring.ui.AuthoringUIImages;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichText;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichTextEditor;
import org.eclipse.epf.authoring.ui.util.EditorsContextHelper;
import org.eclipse.epf.authoring.ui.util.UIHelper;
import org.eclipse.epf.diagram.core.services.DiagramManager;
import org.eclipse.epf.library.edit.LibraryEditResources;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.navigator.ConfigurationsItemProvider;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.edit.validation.IValidator;
import org.eclipse.epf.library.edit.validation.IValidatorFactory;
import org.eclipse.epf.library.ui.LibraryUIText;
import org.eclipse.epf.richtext.RichTextListener;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;


/**
 * The Description page for the process description.
 * 
 * @author Shilpa Toraskar
 * @author Phong Nguyen Le
 * @author Kelvin Low
 * @since 1.0
 * 
 * TODO: Rename this class to ProcessDescriptionPage
 */
public class ProcessDescription extends ProcessFormPage {

	private static final String FORM_PAGE_ID = "processDescriptionPage"; //$NON-NLS-1$

	private static final int DEFAULT_VERTICAL_INDENT = 2;

	protected static final int GENERAL_SECTION_ID = 1;

	protected static final int DETAIL_SECTION_ID = 2;

	private IStructuredContentProvider contentProvider = new AdapterFactoryContentProvider(
			TngAdapterFactory.INSTANCE
					.getNavigatorView_ComposedAdapterFactory()) {
	};

	private ILabelProvider labelProvider = new AdapterFactoryLabelProvider(
			TngAdapterFactory.INSTANCE
					.getNavigatorView_ComposedAdapterFactory());

	protected Section formSection;

	protected Composite sectionComposite;

	private Composite expandedComposite;

	private Text ctrl_name;

	private Text ctrl_presentation_name;

	private Text ctrl_brief_desc, ctrl_external_id;

	private IMethodRichText ctrl_purpose;

	private IMethodRichText ctrl_full_desc;

	private IMethodRichText ctrl_scope, ctrl_usage_notes;

	private IMethodRichText ctrl_alternatives, ctrl_how_to_staff,
			ctrl_key_consideration;

	private IMethodRichText activeControl;

	private Section generalSection, detailSection, configSection;

	protected Composite generalComposite, detailComposite, configComposite;

	protected boolean descExpandFlag = false;

	protected boolean generalSectionExpandFlag = false;

	protected boolean detailSectionExpandFlag = false;

	protected boolean configSectionExpandFlag = false;

	protected IMethodRichTextEditor ctrl_expanded;

	private ImageHyperlink expandLink;

	private Label expandLabel;

	// private String[] variabilityTypes = new String[] {"N/A", "Contributes",
	// "Extends", "Replaces"};
	// private HashMap variabilityElementMap = new HashMap();

	private org.eclipse.swt.widgets.List list_configurations;

	private ListViewer configListViewer;

	protected Button buttonAdd;

	protected Button buttonRemove;

	// private Button buttonEdit;
	protected Button buttonMakeDefault;

	private Text textConfigDescription;

	private ItemProviderAdapter configListAdapter;

	protected ModifyListener modifyListener;

	protected ModifyListener contentModifyListener;

//	private Adapter processListener;

	private String processType;

	private MethodElementEditor editor;

	private ModifyListener nameModifyListener;

	private boolean disposed;

	/**
	 * Creates a new instance.
	 */
	public ProcessDescription(FormEditor editor) {
		super(editor, FORM_PAGE_ID, AuthoringUIResources.descriptionPage_title); 
	}

	/**
	 * @see org.eclipse.ui.forms.editor.createFormContent(IManagedForm)
	 */
	protected void createFormContent(IManagedForm managedForm) {
		// create form toolkit
		super.createFormContent(managedForm);

		processType = LibraryUIText.getUITextLower(process);
		setFormText();

		// create editor content
		createEditorContent(toolkit);

//		// add listener to listen to change in process's data
//		if (processListener == null) {
//			processListener = new AdapterImpl() {
//				public void notifyChanged(Notification msg) {
//					switch (msg.getFeatureID(Process.class)) {
//					case UmaPackage.PROCESS__NAME:
//						refreshElementName(msg.getNewStringValue());
//						break;
//					}
//				}
//			};
//		}
//		process.eAdapters().add(processListener);

		loadData();
		addListeners();

		EditorsContextHelper.setHelp(getPartControl(), processType);
	}

	/**
	 * Creates the editor tab content
	 * 
	 * @param toolkit
	 *            The form toolkit.
	 */
	protected void createEditorContent(FormToolkit toolkit) {
		createFormComposites();
		createGeneralSection();
		createDetailSection();
		createConfigurationSection();

		toolkit.paintBordersFor(generalComposite);
		toolkit.paintBordersFor(detailComposite);
		toolkit.paintBordersFor(configComposite);
		toolkit.paintBordersFor(expandedComposite);
	}

	private void createFormComposites() {
		formSection = toolkit.createSection(form.getBody(), Section.NO_TITLE);
		{
			TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
			formSection.setLayoutData(td);
			formSection.setLayout(new TableWrapLayout());
		}

		// create the composite for the sections
		sectionComposite = toolkit.createComposite(formSection, SWT.NONE);
		sectionComposite.setLayoutData(new TableWrapData());
		sectionComposite.setLayout(new TableWrapLayout());
		formSection.setClient(sectionComposite);

		expandedComposite = toolkit.createComposite(formSection, SWT.NONE);
		{
			TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
			expandedComposite.setLayoutData(td);
			expandedComposite.setLayout(new GridLayout(2, false));
			expandedComposite.setVisible(false);
		}

		PlatformUI.getWorkbench().getHelpSystem().setHelp(expandedComposite,
				AuthoringUIHelpContexts.RICH_TEXT_EDITOR_CONTEXT_ID);

		// Add the expand/collapse hyperlink image.
		expandLink = toolkit.createImageHyperlink(expandedComposite, SWT.NONE);
		expandLink.setImage(AuthoringUIImages.IMG_EXPANDED);
		expandLink.setToolTipText(AuthoringUIResources.closeRTE);
		expandLink.setUnderlined(false);
		expandLink.addHyperlinkListener(new HyperlinkAdapter() {
			public void linkActivated(HyperlinkEvent e) {
				toggle(e);
			}
		});

		// Add the expand/collapse hyperlink text.
		expandLabel = createDecoratedLabel(toolkit, expandedComposite, ""); //$NON-NLS-1$

	}

	/**
	 * Create general section
	 */
	protected void createGeneralSection() {
		// create General Information section
		generalSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
		generalSection.setLayoutData(td);
		generalSection.setText(AuthoringUIText.GENERAL_INFO_SECTION_NAME);
		generalSection.setDescription(MessageFormat.format(
				AuthoringUIText.GENERAL_INFO_SECTION_DESC,
				new String[] { processType }));
		generalSection.setLayout(new GridLayout());

		generalComposite = toolkit.createComposite(generalSection);
		generalComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		generalComposite.setLayout(new GridLayout(3, false));
		generalSection.setClient(generalComposite);

		// name
		ctrl_name = createTextEditWithLabel(toolkit, generalComposite,
				AuthoringUIText.NAME_TEXT);

		// Presentation name
		ctrl_presentation_name = createTextEditWithLabel(toolkit,
				generalComposite, AuthoringUIText.PRESENTATION_NAME_TEXT);

		// brief desc
		ctrl_brief_desc = createTextEditWithLabel2(toolkit, generalComposite,
				AuthoringUIText.BRIEF_DESCRIPTION_TEXT);

		// External Id
		ctrl_external_id = createTextEditWithLabel(toolkit, generalComposite,
				AuthoringUIResources.Process_ExternalID); 

		// Purpose
		ctrl_purpose = createRichTextEditWithLinkForSection(
				toolkit,
				generalComposite,
				AuthoringUIResources.Process_Purpose, 40, 400, GENERAL_SECTION_ID); 

		// // create expanded composite
		// expandGeneralComposite = toolkit.createComposite(generalSection);
		// expandGeneralComposite.setLayoutData(new
		// GridData(GridData.FILL_HORIZONTAL));
		// expandGeneralComposite.setLayout(new GridLayout(2, false));
		// expandGeneralComposite.setVisible(false);
		//		
		// // Hyperlink desc
		// expandGeneralLink =
		// toolkit.createImageHyperlink(expandGeneralComposite, SWT.NONE);
		// expandGeneralLink.setImage(AuthoringUIImages.IMG_EXPANDED);
		// expandGeneralLink.setUnderlined(false);
		// expandGeneralLink.addHyperlinkListener(new HyperlinkAdapter()
		// {
		// public void linkActivated(HyperlinkEvent e)
		// {
		// toggle(e, GENERAL_SECTION_ID);
		// }
		// });
		//		
		// expandGeneralLabel = createLabel(toolkit, expandGeneralComposite,
		// "");

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

	/**
	 * Create detail section
	 *
	 */
	protected void createDetailSection() {
		// create detail section
		detailSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
		detailSection.setLayoutData(td);
		detailSection.setText(AuthoringUIText.DETAIL_SECTION_NAME);
		detailSection.setDescription(MessageFormat.format(
				AuthoringUIText.DETAIL_SECTION_DESC,
				new String[] { processType }));
		detailSection.setLayout(new GridLayout());

		detailComposite = toolkit.createComposite(detailSection);
		detailComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		detailComposite.setLayout(new GridLayout(3, false));
		detailSection.setClient(detailComposite);

		// full description
		ctrl_full_desc = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIText.MAIN_DESCRIPTION_TEXT, 100,
				400, DETAIL_SECTION_ID);

		// scope
		ctrl_scope = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIResources.Process_Scope, 40, 400, DETAIL_SECTION_ID); 

		// usage notes
		ctrl_usage_notes = createRichTextEditWithLinkForSection(
				toolkit,
				detailComposite,
				AuthoringUIResources.Process_UsageNotes, 40, 400, DETAIL_SECTION_ID); 

		// alternatives
		ctrl_alternatives = createRichTextEditWithLinkForSection(
				toolkit,
				detailComposite,
				AuthoringUIResources.Process_Alternatives, 40, 400, DETAIL_SECTION_ID); 

		// how to staff
		ctrl_how_to_staff = createRichTextEditWithLinkForSection(
				toolkit,
				detailComposite,
				AuthoringUIResources.Process_HowToStaff, 40, 400, DETAIL_SECTION_ID); 

		// key consideration
		ctrl_key_consideration = createRichTextEditWithLinkForSection(
				toolkit,
				detailComposite,
				AuthoringUIResources.Process_KeyConsideration, 40, 400, DETAIL_SECTION_ID); 

		// // create expanded composite
		// expandDetailComposite = toolkit.createComposite(detailSection);
		// expandDetailComposite.setLayoutData(new
		// GridData(GridData.FILL_HORIZONTAL));
		// expandDetailComposite.setLayout(new GridLayout(2, false));
		// expandDetailComposite.setVisible(false);
		//		
		// // Hyperlink desc
		// expandDetailLink =
		// toolkit.createImageHyperlink(expandDetailComposite, SWT.NONE);
		// expandDetailLink.setImage(AuthoringUIImages.IMG_EXPANDED);
		// expandDetailLink.setUnderlined(false);
		// expandDetailLink.addHyperlinkListener(new HyperlinkAdapter()
		// {
		// public void linkActivated(HyperlinkEvent e)
		// {
		// toggle(e, DETAIL_SECTION_ID);
		// }
		// });
		//		
		// expandDetailLabel = createLabel(toolkit, expandDetailComposite, "");

	}

	/**
	 * Create configuration section
	 */
	protected void createConfigurationSection() {
		// create Configuration section
		configSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td1 = new TableWrapData(TableWrapData.FILL_GRAB);
		configSection.setLayoutData(td1);
		configSection
				.setText(AuthoringUIResources.processDescription_configurationSectionTitle); 
		configSection
				.setDescription(AuthoringUIResources.processDescription_configurationSectionMessage); 
		configSection.setLayout(new GridLayout());

		configComposite = toolkit.createComposite(configSection);
		configComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		configComposite.setLayout(new GridLayout(2, false));
		configSection.setClient(configComposite);

		Label l_element = toolkit
				.createLabel(
						configComposite,
						AuthoringUIResources.processDescription_configurations); 
		{
			GridData gridData = new GridData(SWT.BEGINNING, SWT.END, true,
					true, 2, 1);
			l_element.setLayoutData(gridData);
		}

		list_configurations = UIHelper.createList(toolkit, configComposite,
				SWT.SINGLE | SWT.BORDER);
		{
			GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
			gridData.verticalIndent = DEFAULT_VERTICAL_INDENT;
			gridData.heightHint = 80;
			gridData.widthHint = 200;
			list_configurations.setLayoutData(gridData);
		}
		configListViewer = new ListViewer(list_configurations);
		configListViewer.setContentProvider(contentProvider);
		configListViewer.setLabelProvider(new LabelProvider() {
			public org.eclipse.swt.graphics.Image getImage(Object element) {
				if (element == process.getDefaultContext()) {
					return AuthoringUIPlugin.getDefault().getSharedImage(
							"icons/checked.gif"); //$NON-NLS-1$
				}
				return null;
			}

			public String getText(Object element) {
				String txt = labelProvider.getText(element);
				if (element == process.getDefaultContext()) {
					txt = txt
							+ " " + AuthoringUIResources.processDescription_default; //$NON-NLS-1$ 
				}
				return txt;
			}
		});

		Composite buttonPanel = toolkit.createComposite(configComposite);
		{
			GridData gridData = new GridData(SWT.CENTER, SWT.CENTER, false,
					false);
			// gridData.widthHint = 80;
			gridData.horizontalIndent = 4;
			buttonPanel.setLayoutData(gridData);
			buttonPanel.setLayout(new GridLayout());

		}
		// Add button
		Button button = toolkit.createButton(buttonPanel, AuthoringUIResources.addButton_text, SWT.NONE); 
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			button.setLayoutData(gridData);
		}
		buttonAdd = button;

		// Remove button
		button = toolkit.createButton(buttonPanel, AuthoringUIResources.removeButton_text, SWT.NONE); 
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			button.setLayoutData(gridData);
		}
		buttonRemove = button;

		// // Edit button
		// button = toolkit.createButton(buttonPanel, "Edit...", SWT.NONE);
		// {
		// GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		// gridData.verticalIndent = DEFAULT_VERTICAL_INDENT * 6;
		// button.setLayoutData(gridData);
		// }
		// buttonEdit = button;
		//		
		//		
		// Make Default button
		button = toolkit.createButton(buttonPanel, AuthoringUIResources.makeDefaultButton_text, SWT.NONE); 
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			button.setLayoutData(gridData);
		}
		buttonMakeDefault = button;

		// configuration description
		Label label = toolkit
				.createLabel(
						configComposite,
						AuthoringUIResources.processDescription_configurationDescription); 
		{
			GridData gridData = new GridData(SWT.BEGINNING, SWT.END, true,
					false, 2, 1);
			label.setLayoutData(gridData);
		}

		textConfigDescription = toolkit.createText(configComposite, ""); //$NON-NLS-1$
		{
			GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
			gridData.verticalIndent = DEFAULT_VERTICAL_INDENT;
			gridData.heightHint = 80;
			gridData.widthHint = 400;
			textConfigDescription.setLayoutData(gridData);
		}
		textConfigDescription.setEditable(false);
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#dispose()
	 */
	public void dispose() {
//		if (processListener != null) {
//			process.eAdapters().remove(processListener);
//		}

		if (configListAdapter != null) {
			configListAdapter.dispose();
		}

		super.dispose();

		if (contentProvider != null) {
			contentProvider.dispose();
		}
		if (labelProvider != null) {
			labelProvider.dispose();
		}
		
		disposed = true;
	}

	/**
	 * Content provider for configurations
	 *
	 */
	class ConfigListItemProvider extends ItemProviderAdapter implements
			IStructuredItemContentProvider {
		
		/**
		 * Creates an instance
		 * @param adapterFactory
		 */
		public ConfigListItemProvider(AdapterFactory adapterFactory) {
			super(adapterFactory);
			process.eAdapters().add(this);
		}

		/**
		 * @see org.eclipse.emf.edit.provider.ItemProviderAdapter#getChildren(java.lang.Object)
		 */
		public java.util.Collection getChildren(Object object) {
			MethodConfiguration defaultConfig = process.getDefaultContext();

			if (defaultConfig != null
					&& !process.getValidContext().contains(defaultConfig)) {
				process.getValidContext().add(0, defaultConfig);
			}
			return process.getValidContext();
		}

		/**
		 * @see org.eclipse.emf.common.notify.impl.AdapterImpl#notifyChanged(org.eclipse.emf.common.notify.Notification)
		 */
		public void notifyChanged(org.eclipse.emf.common.notify.Notification msg) {
			boolean refresh = false;
			switch (msg.getFeatureID(Process.class)) {
			case UmaPackage.PROCESS__VALID_CONTEXT:
				switch (msg.getEventType()) {
				case Notification.ADD:
				case Notification.ADD_MANY:
				case Notification.REMOVE:
				case Notification.REMOVE_MANY:
					refresh = true;
				}
				break;
			case UmaPackage.PROCESS__DEFAULT_CONTEXT:
				refresh = true;
				break;
			}
			if (refresh)
				configListViewer.refresh();
		}
	};

	private void updateModelControls() {
		if ((ctrl_name != null) && (!ctrl_name.isDisposed())) {
			updateControl(ctrl_name, process.getName());
		}
		if ((ctrl_brief_desc != null) && (!ctrl_brief_desc.isDisposed())) {
			updateControl(ctrl_brief_desc, process.getBriefDescription());
		}
		if ((ctrl_presentation_name != null)
				&& (!ctrl_presentation_name.isDisposed())) {
			updateControl(ctrl_presentation_name, process.getPresentationName());
		}
	}

	/**
	 * Updates the Text control iff the newText is different than what's there
	 * @param text
	 * @param newText
	 * @return
	 */
	private boolean updateControl(Text text, String newText) {
		if (!text.getText().equals(TngUtil.checkNull(newText))) {
			text.setText(TngUtil.checkNull(newText));
		}
		return false;
	}
	
	/**
	 * Updates the IMethodRichText control iff the newText is different than what's there
	 * @param text
	 * @param newText
	 * @return
	 */
	private boolean updateControl(IMethodRichText text, String newText) {
		if (!text.getText().equals(TngUtil.checkNull(newText))) {
			text.setText(TngUtil.checkNull(newText));
		}
		return false;
	}

	/**
	 * Loads initial data from model
	 * 
	 */
	public void loadData() {
		org.eclipse.epf.uma.ProcessDescription content = (org.eclipse.epf.uma.ProcessDescription) process
				.getPresentation();

		String externalID = content.getExternalId();
		String purpose = content.getPurpose();
		String fullDesc = content.getMainDescription();
		String scope = content.getScope();
		String usageNotes = content.getUsageNotes();
		String alternatives = content.getAlternatives();
		String howToStaff = content.getHowtoStaff();
		String keyConsideration = content.getKeyConsiderations();

		updateModelControls();

		updateControl(ctrl_external_id, externalID);
		updateControl(ctrl_purpose, purpose);
		updateControl(ctrl_full_desc, fullDesc);
		updateControl(ctrl_scope, scope);
		updateControl(ctrl_usage_notes, usageNotes);
		updateControl(ctrl_alternatives, alternatives);
		updateControl(ctrl_how_to_staff, howToStaff);
		updateControl(ctrl_key_consideration, keyConsideration);

		if (configListAdapter == null) {
			configListAdapter = new ConfigListItemProvider(
					TngAdapterFactory.INSTANCE
							.getNavigatorView_ComposedAdapterFactory());
		}

		configListViewer.setInput(configListAdapter);
	}

	/**
	 * Add listeners
	 * 
	 */
	protected void addListeners() {
		this.editor = ((MethodElementEditor) getEditor());
		final IActionManager actionMgr = editor.getActionManager();
		final org.eclipse.epf.uma.ProcessDescription content = (org.eclipse.epf.uma.ProcessDescription) process
				.getPresentation();
		modifyListener = editor.createModifyListener(process);
		contentModifyListener = editor.createModifyListener(process
				.getPresentation());

		form.addControlListener(new ControlListener() {
			public void controlResized(ControlEvent e) {
				if (!descExpandFlag)
					return;
				if (ctrl_expanded != null) {
					((GridData) ctrl_expanded.getLayoutData()).heightHint = getRichTextEditorHeight();
					((GridData) ctrl_expanded.getLayoutData()).widthHint = getRichTextEditorWidth();
				}
				formSection.layout(true, true);
			}

			public void controlMoved(ControlEvent e) {
			}
		});

		form.getParent().addListener(SWT.Activate, new Listener() {

			public void handleEvent(Event event) {
				if (TngUtil.isLocked(process)) {
					refresh(false);
				} else {
					refresh(true);
				}
			}
		});
		
		nameModifyListener = editor.createModifyListener(process.eContainer(),
				true);
		ctrl_name.addModifyListener(nameModifyListener);
		ctrl_name.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				String oldContent = process.getName();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newName = ctrl_name.getText().trim();
				if (newName.equals(process.getName())) {
					return;
				}

				ProcessComponent procComp = (ProcessComponent) process.eContainer();
				
				String msg = null;
				if (newName.indexOf('&') > -1) { //FIXME: this must go to validator code
					msg = NLS.bind(LibraryEditResources.invalidElementNameError4_msg, newName);
				} else {
					IValidator validator = IValidatorFactory.INSTANCE
					.createNameValidator(
							procComp,
							TngAdapterFactory.INSTANCE
									.getNavigatorView_ComposedAdapterFactory());
					msg = validator.isValid(newName);
				}
				
				if (msg == null) {
					String title = AuthoringUIResources.processDescriptionNameChangeConfirm_title; 
					String message = AuthoringUIResources.processDescriptionNameChangeConfirm_message; 
					if (AuthoringUIPlugin.getDefault().getMsgDialog()
							.displayConfirmation(title, message)) {
						e.doit = true;

						boolean status = actionMgr.doAction(IActionManager.SET,
								process, UmaPackage.eINSTANCE
										.getNamedElement_Name(), ctrl_name
										.getText(), -1);
						if (!status) {
							ctrl_name.setText(process.getName());
							return;
						}
						actionMgr.doAction(IActionManager.SET, procComp,
								UmaPackage.eINSTANCE.getNamedElement_Name(),
								ctrl_name.getText(), -1);

						setFormText();

						// adjust plugin location and save the editor
						//
						BusyIndicator.showWhile(getSite().getShell()
								.getDisplay(), new Runnable() {
							public void run() {
								MethodElementEditor editor = (MethodElementEditor) getEditor();
								editor
										.doSave(new NullProgressMonitor());
								if(editor.isDirty()) {
									// save failed
									//
									return;
								}
								ILibraryPersister.FailSafeMethodLibraryPersister persister = editor
										.getPersister();
								try {
									persister.adjustLocation(process
													.eResource());
									persister.commit();
								} catch (RuntimeException e) {
									persister.rollback();
									throw e;
								}
								// adjust diagram resource as well
								//
								DiagramManager mgr = DiagramManager.getInstance(process, this);
								if(mgr != null) {
									try {
										mgr.updateResourceURI();
									}
									catch(Exception e) {
										AuthoringUIPlugin.getDefault().getLogger().logError(e);
									}
									finally {
										mgr.removeConsumer(this);
									}
								}
							}
						});
					} else {
						ctrl_name.setText(process.getName());
						return;
					}
				} else {
					AuthoringUIPlugin
							.getDefault()
							.getMsgDialog()
							.displayError(
									AuthoringUIResources.renameDialog_title, msg); 
					ctrl_name.setText(process.getName());
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
		
		ctrl_presentation_name.addModifyListener(modifyListener);
		ctrl_presentation_name.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				String oldContent = process.getPresentationName();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				if ((ctrl_presentation_name.getText() != null)
						&& (!ctrl_presentation_name.getText().equals(""))) //$NON-NLS-1$
				{
					e.doit = true;
					if (!ctrl_presentation_name.getText().equals(oldContent)) {

						boolean status = actionMgr
								.doAction(
										IActionManager.SET,
										process,
										UmaPackage.eINSTANCE
												.getMethodElement_PresentationName(),
										ctrl_presentation_name.getText(), -1);
						if (!status) {
							ctrl_presentation_name.setText(oldContent);
							return;
						}
//						LibraryView elementsView = LibraryView.getView();
//						if (elementsView != null)
//							elementsView.getViewer().refresh();
					}
				} else {
					// restore the old name first, then present the error
					// message box
					ctrl_presentation_name.setText(process
							.getPresentationName());
					AuthoringUIPlugin
							.getDefault()
							.getMsgDialog()
							.displayError(
									AuthoringUIResources.editDialog_title, 
									AuthoringUIResources.invalidPresentationNameError_msg); 
					e.doit = false;
					ctrl_presentation_name.getDisplay().asyncExec(
							new Runnable() {
								public void run() {
									ctrl_presentation_name.setFocus();
								}
							});
				}
			}
		});
		ctrl_presentation_name.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_PresentationName());
				// when user tab to this field, select all text
				ctrl_presentation_name.selectAll();

			}
			public void focusLost(FocusEvent e) {
				// clear the selection when the focus of the component is lost 
				if(ctrl_presentation_name.getSelectionCount() > 0){
					ctrl_presentation_name.clearSelection();
				} }
		});

		ctrl_brief_desc.addModifyListener(modifyListener);
		ctrl_brief_desc.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_BriefDescription());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = process.getBriefDescription();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newContent = ctrl_brief_desc.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = actionMgr.doAction(IActionManager.SET,
							process, UmaPackage.eINSTANCE
									.getMethodElement_BriefDescription(),
							newContent, -1);
					if (success) {
						ctrl_brief_desc.setText(newContent);
					}
				}
			}
		});

		ctrl_external_id.addModifyListener(contentModifyListener);
		ctrl_external_id.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getContentDescription_ExternalId());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = content.getExternalId();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_external_id, oldContent)) {
					return;
				}
				String newContent = ctrl_external_id.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = actionMgr.doAction(IActionManager.SET,
							process.getPresentation(), UmaPackage.eINSTANCE
									.getContentDescription_ExternalId(),
							newContent, -1);
					if (success) {
						ctrl_external_id.setText(newContent);
					}
				}
			}

		});

		ctrl_purpose.setModalObject(process.getPresentation());
		ctrl_purpose.setModalObjectFeature(UmaPackage.eINSTANCE
				.getActivityDescription_Purpose());
		ctrl_purpose.addModifyListener(contentModifyListener);
		ctrl_purpose.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_purpose;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getPurpose();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_purpose, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, process
							.getPresentation(), UmaPackage.eINSTANCE
							.getActivityDescription_Purpose(), newContent, -1);
				}
			}
		});

		ctrl_full_desc.setModalObject(process.getPresentation());
		ctrl_full_desc.setModalObjectFeature(UmaPackage.eINSTANCE
				.getContentDescription_MainDescription());
		ctrl_full_desc.addModifyListener(contentModifyListener);
		ctrl_full_desc.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_full_desc;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getMainDescription();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_full_desc, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, process
							.getPresentation(), UmaPackage.eINSTANCE
							.getContentDescription_MainDescription(),
							newContent, -1);
				}
			}
		});

		ctrl_scope.setModalObject(process.getPresentation());
		ctrl_scope.setModalObjectFeature(UmaPackage.eINSTANCE
				.getProcessDescription_Scope());
		ctrl_scope.addModifyListener(contentModifyListener);
		ctrl_scope.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_scope;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getScope();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_scope, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, process
							.getPresentation(), UmaPackage.eINSTANCE
							.getProcessDescription_Scope(), newContent, -1);
				}
			}
		});

		ctrl_usage_notes.setModalObject(process.getPresentation());
		ctrl_usage_notes.setModalObjectFeature(UmaPackage.eINSTANCE
				.getProcessDescription_UsageNotes());
		ctrl_usage_notes.addModifyListener(contentModifyListener);
		ctrl_usage_notes.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_usage_notes;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getUsageNotes();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_usage_notes, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr
							.doAction(IActionManager.SET, process
									.getPresentation(), UmaPackage.eINSTANCE
									.getProcessDescription_UsageNotes(),
									newContent, -1);
				}
			}
		});

		ctrl_alternatives.setModalObject(process.getPresentation());
		ctrl_alternatives.setModalObjectFeature(UmaPackage.eINSTANCE
				.getActivityDescription_Alternatives());
		ctrl_alternatives.addModifyListener(contentModifyListener);
		ctrl_alternatives.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_alternatives;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getAlternatives();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_alternatives, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, process
							.getPresentation(), UmaPackage.eINSTANCE
							.getActivityDescription_Alternatives(), newContent,
							-1);
				}
			}
		});

		ctrl_how_to_staff.setModalObject(process.getPresentation());
		ctrl_how_to_staff.setModalObjectFeature(UmaPackage.eINSTANCE
				.getActivityDescription_HowtoStaff());
		ctrl_how_to_staff.addModifyListener(contentModifyListener);
		ctrl_how_to_staff.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_how_to_staff;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getHowtoStaff();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_how_to_staff, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, process
							.getPresentation(), UmaPackage.eINSTANCE
							.getActivityDescription_HowtoStaff(), newContent,
							-1);
				}
			}
		});

		ctrl_key_consideration.setModalObject(process.getPresentation());
		ctrl_key_consideration.setModalObjectFeature(UmaPackage.eINSTANCE
				.getContentDescription_KeyConsiderations());
		ctrl_key_consideration.addModifyListener(contentModifyListener);
		ctrl_key_consideration.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_key_consideration;
				if (!control.getModified()) {
					return;
				}
				String oldContent = content.getKeyConsiderations();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						ctrl_key_consideration, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					actionMgr.doAction(IActionManager.SET, process
							.getPresentation(), UmaPackage.eINSTANCE
							.getContentDescription_KeyConsiderations(),
							newContent, -1);
				}
			}
		});

		configListViewer
				.addSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						IStructuredSelection selection = (IStructuredSelection) configListViewer
								.getSelection();
						if (selection.size() == 1) {
							MethodConfiguration config = ((MethodConfiguration) selection
									.getFirstElement());
							if (config == process.getDefaultContext()) {
								buttonRemove.setEnabled(false);
							} else {
								buttonRemove.setEnabled(true);
							}
							String desc = config.getBriefDescription();
							if (desc == null) {
								desc = ""; //$NON-NLS-1$
							}
							textConfigDescription.setText(desc);
						}
					}
				});

		buttonAdd.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				MethodLibrary lib = UmaUtil.getMethodLibrary(process);

				final ConfigurationsItemProvider input = new ConfigurationsItemProvider(
						TngAdapterFactory.INSTANCE
								.getNavigatorView_ComposedAdapterFactory(),
						lib, ""); //$NON-NLS-1$

				ListSelectionDialog dlg = new ListSelectionDialog(
						Display.getCurrent().getActiveShell(),
						input,
						contentProvider,
						labelProvider,
						AuthoringUIResources.ProcessDescription_selectConfiguration) { 
					protected Control createDialogArea(Composite parent) {
						Control control = super.createDialogArea(parent);
						getViewer().addFilter(new ViewerFilter() {

							public boolean select(Viewer viewer,
									Object parentElement, Object element) {
								// int count =
								// configListViewer.getList().getItemCount();
								// for(int i = 0; i < count; i++) {
								// if(element ==
								// configListViewer.getElementAt(i)) return
								// false;
								// }
								// return true;
								return !process.getValidContext().contains(
										element);
							}

						});
						return control;
					}

					public boolean close() {
						input.dispose();
						return super.close();
					}
				};
				if (dlg.open() == Window.OK) {
					Object obs[] = dlg.getResult();
					MethodConfiguration config;
					// get default configuration
//					MethodConfiguration defaultConfig = process
//							.getDefaultContext();
					for (int i = 0; i < obs.length; i++) {
						config = (MethodConfiguration) obs[i];
						if (config != null) {
//								|| checkValidityForSuperSet(defaultConfig,
//										config)) {
							actionMgr.doAction(IActionManager.ADD, process,
									UmaPackage.eINSTANCE
											.getProcess_ValidContext(), config,
									-1);
						} else {
							AuthoringUIPlugin
									.getDefault()
									.getMsgDialog()
									.displayError(
											AuthoringUIResources.addConfigErrorDialog_title, 
											AuthoringUIResources.bind(AuthoringUIResources.invalidConfigError_msg, config.getName())); 
						}
					}
				}
			}
		});

		buttonRemove.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				IStructuredSelection selection = (IStructuredSelection) configListViewer
						.getSelection();
				actionMgr.doAction(IActionManager.REMOVE_MANY, process,
						UmaPackage.eINSTANCE.getProcess_ValidContext(),
						selection.toList(), -1);
			}
		});

		buttonMakeDefault.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				IStructuredSelection selection = (IStructuredSelection) configListViewer
						.getSelection();
				if (selection.size() == 1) {
					MethodConfiguration currentConfig = process
							.getDefaultContext();

					if (currentConfig != selection.getFirstElement()) {
						boolean isValid = true;
						List validContext = process.getValidContext();
						for (Iterator itor = validContext.iterator(); itor
								.hasNext();) {
							MethodConfiguration config = (MethodConfiguration) itor
									.next();
							if (!(checkValidityForSuperSet(
									(MethodConfiguration) selection
											.getFirstElement(), config))) {
								isValid = false;
								break;
							}
						}
						if (isValid) {
							boolean status = actionMgr.doAction(
									IActionManager.SET, process,
									UmaPackage.eINSTANCE
											.getProcess_DefaultContext(),
									selection.getFirstElement(), -1);
							if (!status)
								return;
							buttonRemove.setEnabled(false);
						} else {
							String selectedConfigName = ((MethodConfiguration) selection
									.getFirstElement()).getName();
							AuthoringUIPlugin
									.getDefault()
									.getMsgDialog()
									.displayError(
											AuthoringUIResources.setDefaultConfigErrorDialog_title, 
											AuthoringUIResources.bind(AuthoringUIResources.setDefaultConfigError_msg, selectedConfigName)); 
						}
					}
				}
			}
		});
	}

	/**
	 * Refresh all the controls
	 * 
	 * @param editable
	 */
	protected void refresh(boolean editable) {
		if (!ctrl_name.isDisposed()) {
			ctrl_name.setEditable(editable);
		}
		if (!ctrl_presentation_name.isDisposed()) {
			ctrl_presentation_name.setEditable(editable);
		}
		if (!ctrl_brief_desc.isDisposed()) {
			ctrl_brief_desc.setEditable(editable);
		}
		if (!ctrl_purpose.isDisposed()) {
			ctrl_purpose.setEditable(editable);
		}
		if (!ctrl_full_desc.isDisposed()) {
			ctrl_full_desc.setEditable(editable);
		}
		if (!ctrl_alternatives.isDisposed()) {
			ctrl_alternatives.setEditable(editable);
		}
		if (!ctrl_external_id.isDisposed())
			ctrl_external_id.setEditable(editable);
		if (!ctrl_key_consideration.isDisposed())
			ctrl_key_consideration.setEditable(editable);
		if (!ctrl_how_to_staff.isDisposed())
			ctrl_how_to_staff.setEditable(editable);
		if (!ctrl_scope.isDisposed())
			ctrl_scope.setEditable(editable);
		if (!ctrl_usage_notes.isDisposed())
			ctrl_usage_notes.setEditable(editable);
		if (ctrl_expanded != null && !ctrl_expanded.isDisposed()) {
			ctrl_expanded.setEditable(editable);
		}
		if (!buttonAdd.isDisposed())
			buttonAdd.setEnabled(editable);
		if (!buttonMakeDefault.isDisposed())
			buttonMakeDefault.setEnabled(editable);
		if (!buttonRemove.isDisposed())
			buttonRemove.setEnabled(editable);
	}

	/**
	 * Toggle Description control to expand and control state
	 * 
	 */
	protected void toggle(HyperlinkEvent e, int id) {
		// TODO- we should combine these methods into one. One way to do it,
		// dispoing
		// ctrl_expanded every time it collapses and creating it when we expand.
		// At present, there is no method to dispose
		toggle(e);
		// if (id == GENERAL_SECTION_ID)
		// {
		// toggleGeneralSection(e);
		// }
		// if (id == DETAIL_SECTION_ID)
		// {
		// toggleDetailSection(e);
		// }
	}

	protected void toggle(HyperlinkEvent e) {
		if (ctrl_expanded == null) {
			ctrl_expanded = createRichTextEditor(toolkit, expandedComposite,
					SWT.MULTI | SWT.WRAP | SWT.V_SCROLL,
					GridData.FILL_VERTICAL, getRichTextEditorHeight(),
					getRichTextEditorWidth(), 2,
					expandLabel);
			ctrl_expanded.addModifyListener(contentModifyListener);
		}

		if (descExpandFlag) {
			expandedComposite.setVisible(false);
			sectionComposite.setVisible(true);
			formSection.setClient(sectionComposite);
			enableSections(true);
			IMethodRichText richText = getActiveRichTextControl();
			richText.setText(ctrl_expanded.getText());
			for (Iterator i = richText.getListeners(); i.hasNext();) {
				RichTextListener listener = (RichTextListener) i.next();
				ctrl_expanded.removeListener(listener.getEventType(), listener
						.getListener());
			}
			if (ctrl_expanded.getModified()) {
				((MethodElementEditor) getEditor())
						.saveModifiedRichText(ctrl_expanded);
			}
			editor.setFocus();
		} else {
			sectionComposite.setVisible(false);
			expandedComposite.setVisible(true);
			formSection.setClient(expandedComposite);
			enableSections(false);
			expandLabel.setText((String) ((ImageHyperlink) e.getSource())
					.getData("Title")); //$NON-NLS-1$
			IMethodRichText richText = (IMethodRichText) e.getHref();
			ctrl_expanded.setInitialText(richText.getText());
			ctrl_expanded.setModalObject(richText.getModalObject());
			ctrl_expanded.setModalObjectFeature(richText
					.getModalObjectFeature());
			ctrl_expanded.setFindReplaceAction(richText.getFindReplaceAction());
			for (Iterator i = richText.getListeners(); i.hasNext();) {
				RichTextListener listener = (RichTextListener) i.next();
				ctrl_expanded.addListener(listener.getEventType(), listener
						.getListener());
			}
			boolean editable = !TngUtil.isLocked(process);
			ctrl_expanded.setEditable(editable);
			if (editable) {
				ctrl_expanded.setFocus();
			}
			setActiveRichTextControl(richText);
		}
		form.getBody().layout(true, true);
		descExpandFlag = !descExpandFlag;
	}

	/**
	 * Checks whether default configuration is superset of selected
	 * configuration or not
	 * 
	 * @param config
	 * @return
	 */
	private boolean checkValidityForSuperSet(MethodConfiguration defaultConfig,
			MethodConfiguration config) {
		// get default plugins and packages
		List defaultPlugins = defaultConfig.getMethodPluginSelection();
		List defaultPackages = defaultConfig.getMethodPackageSelection();

		// get confiugration plugins and pacakges
		List packages = config.getMethodPackageSelection();
		List plugins = config.getMethodPluginSelection();
		boolean pluginContains = false;
		boolean packageContains = false;
		if ((plugins != null) ) {
			pluginContains = defaultPlugins.containsAll(plugins);
		}
		if ((packages != null) ) {
			packageContains = defaultPackages.containsAll(packages);
		}

		return (pluginContains) && (packageContains);
	}

	/**
	 * Set active rich text control
	 * 
	 * @param ctrl
	 */
	private void setActiveRichTextControl(IMethodRichText ctrl) {
		activeControl = ctrl;
	}

	/**
	 * Get Active Rich text control
	 * 
	 * @return
	 * 		Rich text control
	 */
	private IMethodRichText getActiveRichTextControl() {
		return activeControl;
	}

	protected void enableSections(boolean enable) {
		generalSection.setVisible(enable);
		if (enable) {
			generalSection.setExpanded(generalSectionExpandFlag);
		} else {
			generalSectionExpandFlag = generalSection.isExpanded();
			generalSection.setExpanded(enable);
		}

		detailSection.setVisible(enable);
		if (enable) {
			detailSection.setExpanded(detailSectionExpandFlag);
		} else {
			detailSectionExpandFlag = detailSection.isExpanded();
			detailSection.setExpanded(enable);
		}
		configSection.setVisible(enable);
		if (enable) {
			configSection.setExpanded(configSectionExpandFlag);
		} else {
			configSectionExpandFlag = configSection.isExpanded();
			configSection.setExpanded(enable);
		}
	}

	/**
	 * Set name for the form
	 */
	public void setFormText() {

		form.setText(LibraryUIText.getUIText(process)
				+ ": " + methodElement.getName()); //$NON-NLS-1$
	}

	/**
	 * Refresh form name
	 * @param newName
	 */
	public void refreshElementName(final String newName) {
		if (newName != null) {
			if ((ctrl_name != null) && !(ctrl_name.isDisposed())) {
				Runnable runnable = new Runnable() {

					public void run() {
						ctrl_name.removeModifyListener(nameModifyListener);
						ctrl_name.setText(newName);
						ctrl_name.addModifyListener(nameModifyListener);
						setFormText();
					}
					
				};
				if (ctrl_name.getDisplay().getThread() == Thread.currentThread()) {
					runnable.run();
				} else {
					ctrl_name.getDisplay().syncExec(runnable);
				}

			}
		}
	}
}
