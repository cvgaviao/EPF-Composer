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
import java.util.Iterator;
import java.util.List;

import org.eclipse.epf.authoring.ui.AuthoringUIHelpContexts;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditorInput;
import org.eclipse.epf.authoring.ui.util.UIHelper;
import org.eclipse.epf.library.edit.LibraryEditResources;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.util.MethodElementUtil;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.ui.LibraryUIText;
import org.eclipse.epf.uma.ContentElement;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
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
 * The description page for the Content Package editor.
 * 
 * @author Shilpa Toraskar
 * @author Kelvin Low
 * @since 1.0
 */
public class ContentPackageDescriptionPage extends FormPage implements IRefreshable {

	private static final String FORM_PREFIX = LibraryUIText.TEXT_CONTENT_PACKAGE
			+ ": "; //$NON-NLS-1$

	private Text ctrl_name;
	
	private Text ctrl_presentation_name;

	private Text ctrl_brief_desc;

	private CheckboxTableViewer ctrl_dependency;

	private ScrolledForm form;

	private ContentPackage contentPackage;

	private org.eclipse.epf.authoring.ui.editors.MethodElementEditor.ModifyListener modelModifyListener;

	/**
	 * Creates a new instance.
	 */
	public ContentPackageDescriptionPage(FormEditor editor) {
		super(editor, AuthoringUIText.DESCRIPTION_PAGE_TITLE,
				AuthoringUIText.DESCRIPTION_PAGE_TITLE);
	}

	/**
	 * @see org.eclipse.ui.forms.editor.FormPage#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);

		// Retrieve the ContentPackage object from the Editor input.
		MethodElementEditorInput methodElementInput = (MethodElementEditorInput) input;
		Object obj = methodElementInput.getMethodElement();
		contentPackage = (ContentPackage) obj;
	}

	/**
	 * @see org.eclipse.ui.forms.editor.createFormContent(IManagedForm)
	 */
	protected void createFormContent(IManagedForm managedForm) {
		// Create the form toolkit.
		form = managedForm.getForm();
		FormToolkit toolkit = managedForm.getToolkit();
		form.setText(FORM_PREFIX + contentPackage.getName());
		TableWrapLayout layout = new TableWrapLayout();
		form.getBody().setLayout(layout);

		// Create the General Informaiton section.
		Section generalSection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td = new TableWrapData(TableWrapData.FILL_GRAB);
		generalSection.setLayoutData(td);
		generalSection.setText(AuthoringUIText.GENERAL_INFO_SECTION_NAME);
		generalSection.setDescription(MessageFormat.format(
				AuthoringUIText.GENERAL_INFO_SECTION_DESC,
				new String[] { LibraryUIText.getUITextLower(contentPackage) }));
		generalSection.setLayout(new GridLayout());

		Composite generalComposite = toolkit.createComposite(generalSection);
		generalComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		generalComposite.setLayout(new GridLayout(2, false));
		generalSection.setClient(generalComposite);

		PlatformUI.getWorkbench().getHelpSystem().setHelp(
				generalComposite.getParent().getParent(),
				AuthoringUIHelpContexts.CONTENT_PACKAGE_EDITOR_ALL_CONTEXT); 

		// name
		Label l_name = toolkit.createLabel(generalComposite,
				AuthoringUIText.NAME_TEXT);
		{
			GridData gridData = new GridData(GridData.BEGINNING);
			l_name.setLayoutData(gridData);
		}

		ctrl_name = toolkit.createText(generalComposite, ""); //$NON-NLS-1$
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL
					| GridData.GRAB_HORIZONTAL);
			ctrl_name.setLayoutData(gridData);
		}

		// presentation name 
		Label l_presentation_name = toolkit.createLabel(generalComposite,
				AuthoringUIText.PRESENTATION_NAME_TEXT);
		{
			GridData gridData = new GridData(GridData.BEGINNING);
			l_presentation_name.setLayoutData(gridData);
		}

		ctrl_presentation_name = toolkit.createText(generalComposite, ""); //$NON-NLS-1$
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL
					| GridData.GRAB_HORIZONTAL);
			ctrl_presentation_name.setLayoutData(gridData);
		}
		
		// brief desc
		Label l_brief_desc = toolkit.createLabel(generalComposite,
				AuthoringUIText.BRIEF_DESCRIPTION_TEXT);
		{
			GridData gridData = new GridData(GridData.BEGINNING);
			l_brief_desc.setLayoutData(gridData);
		}

		ctrl_brief_desc = toolkit.createText(generalComposite,
				"", SWT.MULTI | SWT.WRAP | SWT.V_SCROLL); //$NON-NLS-1$
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			gridData.heightHint = 40;
			gridData.widthHint = 300;
			ctrl_brief_desc.setLayoutData(gridData);
		}

		Section dependencySection = toolkit.createSection(form.getBody(),
				Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED
						| Section.TITLE_BAR);
		TableWrapData td1 = new TableWrapData(TableWrapData.FILL_GRAB);
		dependencySection.setLayoutData(td1);
		dependencySection.setText(AuthoringUIText.DEPENDENCIES_SECTION_NAME);
		dependencySection
				.setDescription(AuthoringUIText.DEPENDENCIES_SECTION_DESC);
		dependencySection.setLayout(new GridLayout());

		Composite dependencyComposite = toolkit
				.createComposite(dependencySection);
		dependencyComposite
				.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		dependencyComposite.setLayout(new GridLayout(2, false));
		dependencySection.setClient(dependencyComposite);

		
		Table ctrl_table = toolkit.createTable(dependencyComposite, SWT.V_SCROLL
				| SWT.CHECK | SWT.READ_ONLY | SWT.COLOR_BLUE);
		{
			GridData gridData = new GridData(GridData.BEGINNING
					| GridData.FILL_BOTH);
			gridData.heightHint = 100;
			ctrl_table.setLayoutData(gridData);
		}

		ctrl_dependency = new CheckboxTableViewer(ctrl_table);

		ILabelProvider labelProvider = new LabelProvider() {
			public String getText(Object element) {
				if (element instanceof ContentPackage) {
					return TngUtil.getLabelWithPath((ContentPackage)element);
				} else {
					return element.toString();
				}
			}
		};
		ctrl_dependency.setLabelProvider(labelProvider);
		ctrl_dependency.setContentProvider(new IStructuredContentProvider() {
			public void dispose() {
			}
			
			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}
			
			public Object[] getElements(Object inputElement) {
				if (inputElement instanceof ContentPackage) {
					return getDependenciesPackages(contentPackage).toArray();
				} else {
					return Collections.EMPTY_LIST.toArray();
				}
			}
		});
		ctrl_dependency.setSorter(new ViewerSorter());
		ctrl_dependency.setInput(contentPackage);

		ctrl_dependency.setAllChecked(true);
		ctrl_dependency.setAllGrayed(true);

		toolkit.paintBordersFor(generalComposite);
		toolkit.paintBordersFor(dependencyComposite);

		// load data
		loadData();
		addListeners();

		// set focus on the name attribute
		Display display = form.getBody().getDisplay();
		if (!(display == null || display.isDisposed())) {
			display.asyncExec(new Runnable() {
				public void run() {
					ctrl_name.setFocus();
					ctrl_name.setSelection(0, ctrl_name.getText().length());
				}
			});
		}
	}

	/**
	 * Add listeners
	 * 
	 */
	private void addListeners() {
		final MethodElementEditor editor = (MethodElementEditor) getEditor();
		modelModifyListener = editor.createModifyListener(contentPackage);

		form.addListener(SWT.Activate, new Listener() {
			public void handleEvent(Event e) {
				// refreshViewers();
				ctrl_dependency.refresh();
				ctrl_dependency.setAllChecked(true);
				ctrl_dependency.setAllGrayed(true);
				if (TngUtil.isLocked(contentPackage)) {
					enableControls(false);
				} else {
					enableControls(true);
				}
			}
		});

		ctrl_name.addModifyListener(modelModifyListener);
		ctrl_name.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getNamedElement_Name());
			}

			public void focusLost(FocusEvent e) {
				final Collection eClasses = Collections
						.singleton(UmaPackage.eINSTANCE.getMethodPackage());
				String oldContent = contentPackage.getName();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String msg = null ;
				String newName = ctrl_name.getText();
				if(newName != null) {
					newName = newName.trim();					
					// 178462
					if (oldContent.indexOf("&") < 0 && newName.indexOf("&") > -1) { //$NON-NLS-1$ //$NON-NLS-2$
						msg = NLS
								.bind(
										LibraryEditResources.invalidElementNameError4_msg,
										newName);
					} else {
						msg = TngUtil.checkName(contentPackage, newName,
								eClasses);
					}
				}
				if (msg == null) {
					if (!newName.equals(contentPackage.getName())) {
						ctrl_name.setText(newName);
						editor.getActionManager().doAction(IActionManager.SET,
								contentPackage,
								UmaPackage.eINSTANCE.getNamedElement_Name(),
								newName, -1);
					}
				} else {
					AuthoringUIPlugin
							.getDefault()
							.getMsgDialog()
							.displayError(
									AuthoringUIResources.renameDialog_title, msg); 
					ctrl_name.setText(contentPackage.getName());
					ctrl_name.getDisplay().asyncExec(new Runnable() {
						public void run() {
							ctrl_name.setFocus();
							ctrl_name.selectAll();
						}
					});
				}
			}
		});
		
		// add presentation_name listener
		ctrl_presentation_name.addModifyListener(modelModifyListener);
		ctrl_presentation_name.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_PresentationName());
				// when user tab to this field, select all text
				ctrl_presentation_name.selectAll();
			}

			public void focusLost(FocusEvent e) {
				String oldContent = contentPackage.getPresentationName();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newName = ctrl_presentation_name.getText();
				if (!newName.equals(contentPackage.getPresentationName())) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							contentPackage,
							UmaPackage.eINSTANCE
									.getMethodElement_PresentationName(),
							newName, -1);
					if (success) {
						ctrl_presentation_name.setText(newName);
					}
				}
				// clear the selection when the focus of the component is lost 
				if(ctrl_presentation_name.getSelectionCount() > 0){
					ctrl_presentation_name.clearSelection();
				} 
			}
		});
		
		ctrl_brief_desc.addModifyListener(modelModifyListener);
		ctrl_brief_desc.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				((MethodElementEditor) getEditor()).setCurrentFeatureEditor(e.widget,
						UmaPackage.eINSTANCE.getMethodElement_BriefDescription());
			}

			public void focusLost(FocusEvent e) {
				String oldContent = contentPackage.getBriefDescription();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						e.widget, oldContent)) {
					return;
				}
				String newName = ctrl_brief_desc.getText();
				if (!newName.equals(contentPackage.getBriefDescription())) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							contentPackage,
							UmaPackage.eINSTANCE
									.getMethodElement_BriefDescription(),
							newName, -1);
					if (success) {
						ctrl_brief_desc.setText(newName);
					}
				}
			}
		});

		ctrl_dependency.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent event) {
				Object obj = event.getSource();
				((CheckboxTableViewer) obj).setAllChecked(true);
			}
		});

	}

	protected void enableControls(boolean editable) {
		ctrl_name.setEditable(editable);
		ctrl_presentation_name.setEditable(editable);
		ctrl_brief_desc.setEditable(editable);
	}

	/**
	 * Loads initial data from model
	 */
	private void loadData() {
		String name = contentPackage.getName();
		String presentation_name = contentPackage.getPresentationName();
		String desc = contentPackage.getBriefDescription();


		ctrl_name.setText(name == null ? "" : name); //$NON-NLS-1$
		ctrl_name.selectAll();
		ctrl_presentation_name.setText(desc == null ? "" : presentation_name); //$NON-NLS-1$
		ctrl_brief_desc.setText(desc == null ? "" : desc); //$NON-NLS-1$
	}

	/**
	 * Get dependency packages for the given content package
	 * @return
	 * 		List of dependency packages
	 */
	public Collection<ContentPackage> getDependenciesPackages(ContentPackage contentPackage) {

		List elements = new ArrayList();
		for (Iterator iter = contentPackage.getContentElements().iterator(); iter
				.hasNext();) {
			ContentElement contentElement = (ContentElement) iter.next();
			if (contentElement.getVariabilityBasedOnElement() != null) {
				elements.add((ContentElement) contentElement
						.getVariabilityBasedOnElement());
			}
			elements.addAll(MethodElementUtil
					.getSelectedGuidances(contentElement));

			if (contentElement instanceof Role) {
/*				if (AssociationHelper
						.getModifiedWorkProducts((Role) contentElement) != null) {
					elements.addAll(AssociationHelper
							.getModifiedWorkProducts((Role) contentElement));
				}
				if (AssociationHelper.getPrimaryTasks((Role) contentElement) != null) {
					elements.addAll(AssociationHelper
							.getPrimaryTasks((Role) contentElement));
				}
				if (AssociationHelper.getSecondaryTasks((Role) contentElement) != null) {
					elements.addAll(AssociationHelper
							.getSecondaryTasks((Role) contentElement));
				}*/
			}
			if (contentElement instanceof Task) {
				if (((Task) contentElement).getPerformedBy() != null) {
					elements.addAll(((Task) contentElement).getPerformedBy());
				}
				if (((Task) contentElement).getAdditionallyPerformedBy() != null) {
					elements.addAll(((Task) contentElement)
							.getAdditionallyPerformedBy());
				}
				if (((Task) contentElement).getMandatoryInput() != null) {
					elements
							.addAll(((Task) contentElement).getMandatoryInput());
				}
				if (((Task) contentElement).getOptionalInput() != null) {
					elements.addAll(((Task) contentElement).getOptionalInput());
				}
				if (((Task) contentElement).getOutput() != null) {
					elements.addAll(((Task) contentElement).getOutput());
				}
			}
			if (contentElement instanceof WorkProduct) {
/*				if (AssociationHelper
						.getModifiedBy((WorkProduct) contentElement) != null) {
					elements.addAll(AssociationHelper
							.getModifiedBy((WorkProduct) contentElement));
				}
				if (AssociationHelper
						.getMandatoryInputToTasks((WorkProduct) contentElement) != null) {
					elements
							.addAll(AssociationHelper
									.getMandatoryInputToTasks((WorkProduct) contentElement));
				}
				if (AssociationHelper
						.getOutputtingTasks((WorkProduct) contentElement) != null) {
					elements.addAll(AssociationHelper
							.getOutputtingTasks((WorkProduct) contentElement));
				}
				List list = AssociationHelper.getResponsibleRoles((WorkProduct) contentElement);
				if (list != null) {
					elements.addAll(list);
				}*/
			}

		}

		List<ContentPackage> cpList = new ArrayList<ContentPackage>();
		for (int i = 0; i < elements.size(); i++) {
			Object object = ((ContentElement) elements.get(i)).eContainer();
			if (object instanceof ContentPackage) {
				if (!contentPackage.equals(object)) {
					if (!cpList.contains((ContentPackage)object)) {
						cpList.add((ContentPackage)object);
					}
				}
			}
		}
		return cpList;
	}	

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.IRefreshable#refreshName(java.lang.String)
	 */
	public void refreshName(String newName) {
		if (newName != null) {
			if ((ctrl_name != null) && !(ctrl_name.isDisposed())) {
				ctrl_name.removeModifyListener(modelModifyListener);
				ctrl_name.setText(newName);
				ctrl_name.addModifyListener(modelModifyListener);
				UIHelper.setFormText(form, contentPackage);
			}
		}
	}

}
