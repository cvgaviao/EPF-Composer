/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.authoring.ui.forms;

import java.util.Set;

import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.util.UIHelper;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.util.WorkProductPropUtil;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;

/**
 * 
 * @author achen
 * @since 7.5.1
 */
public class WorkProductStatesPage extends BaseFormPage {
	private static final String FORM_PAGE_ID = "workProductStatesPage"; //$NON-NLS-1$
	
	private WorkProduct workProduct;
	private IActionManager actionMgr;
	
	private Section statesSection;
	private Composite statesComposite;
	private Text ctrl_name;
	private Button ctrl_add;
	private Table ctrl_states;
	private TableViewer statesTableViewer;
	private IStructuredContentProvider statesViewerContentProvider;
	private ITableLabelProvider statesViewerLabelProvider;
	private Button ctrl_delete;

	public WorkProductStatesPage(FormEditor editor) {
		super(editor, FORM_PAGE_ID, AuthoringUIText.WORK_PRODUCT_STATES_PAGE_TITLE);
	}
	
	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		workProduct = (WorkProduct) contentElement;
		actionMgr = ((MethodElementEditor) getEditor()).getActionManager();
	}
	
	protected void createFormContent(IManagedForm managedForm) {
		super.createFormContent(managedForm);
		UIHelper.setFormText(form, contentElement);
		
		statesSection = toolkit.createSection(form.getBody(),
				Section.TITLE_BAR | Section.DESCRIPTION | Section.TWISTIE | Section.EXPANDED);
		statesSection.setLayoutData(new TableWrapData(TableWrapData.FILL_GRAB));
		statesSection.setText(AuthoringUIText.STATES_SECTION_NAME);
		statesSection.setDescription(AuthoringUIText.STATES_SECTION_DESC);
		statesSection.setLayout(new GridLayout());

		statesComposite = toolkit.createComposite(statesSection);
		statesComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		statesComposite.setLayout(new GridLayout());
		statesSection.setClient(statesComposite);
		
		createStatesArea(statesComposite);		
		toolkit.paintBordersFor(statesComposite);
		
		addListeners();
		loadData();
		updateControls();
	}
	
	protected void createStatesArea(Composite parent) {
		Composite nameComposite = toolkit.createComposite(parent);
		nameComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		nameComposite.setLayout(new GridLayout(3, false));		
		toolkit.createLabel(nameComposite, AuthoringUIText.STATES_NAME_TEXT);		
		ctrl_name = toolkit.createText(nameComposite, null, SWT.SINGLE);
		{
			GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
			gridData.widthHint = 300;
			ctrl_name.setLayoutData(gridData);
		}
		ctrl_add = toolkit.createButton(nameComposite, AuthoringUIText.STATES_ADD_TEXT, SWT.NONE);
		{
			GridData gridData = new GridData();
			gridData.widthHint = 70;
			ctrl_add.setLayoutData(gridData);
			
		}

		Composite listComposite = toolkit.createComposite(parent);
		listComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		listComposite.setLayout(new GridLayout(4, false));
		Label state = toolkit.createLabel(listComposite, AuthoringUIText.STATES_TEXT);
		{
			GridData gridData = new GridData();
			gridData.horizontalSpan = 4;
			state.setLayoutData(gridData);
		}
		ctrl_states = toolkit.createTable(listComposite, SWT.MULTI);
		{
			GridData gridData = new GridData(GridData.FILL_VERTICAL);
			gridData.horizontalSpan = 3;
			gridData.widthHint = 500;
			gridData.heightHint = 150;
			ctrl_states.setLayoutData(gridData);
		}
		ctrl_delete = toolkit.createButton(listComposite, AuthoringUIText.STATES_DELETE_TEXT, SWT.NONE);
		{
			GridData gridData = new GridData();
			gridData.widthHint = 70;
			ctrl_delete.setLayoutData(gridData);
		}

		initProviders();
		
		statesTableViewer = new TableViewer(ctrl_states);
		statesTableViewer.setContentProvider(statesViewerContentProvider);
		statesTableViewer.setLabelProvider(statesViewerLabelProvider);
		statesTableViewer.setComparator(new ViewerComparator() {
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof Constraint) && (e2 instanceof Constraint)) {
					String name1 = ((Constraint)e1).getBody();
					String name2 = ((Constraint)e2).getBody();					
					return getComparator().compare(name1, name2);
				}
				
				return 0;
			}
		});
		
		toolkit.paintBordersFor(nameComposite);
		toolkit.paintBordersFor(listComposite);
	}
	
	private void initProviders() {
		statesViewerContentProvider = new AdapterFactoryContentProvider(
				TngAdapterFactory.INSTANCE.getNavigatorView_ComposedAdapterFactory()) {
			public Object[] getElements(Object object) {
				if (object instanceof WorkProduct) {
					WorkProduct wp = (WorkProduct)object;
					Set<Constraint> states = WorkProductPropUtil.getWorkProductPropUtil(actionMgr).getAllStates(wp);
					return states.toArray();
				}
				
				return new Object[0];
			}
		};

		statesViewerLabelProvider = new AdapterFactoryLabelProvider(
				TngAdapterFactory.INSTANCE.getNavigatorView_ComposedAdapterFactory()) {
			public String getColumnText(Object element, int columnIndex) {
				if (element instanceof Constraint) {
					return ((Constraint)element).getBody();
				}
				
				return null;
			}
		};
	}
	
	private void addListeners() {
		ctrl_add.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				createState();
				updateControls();
			}
		});
		
		ctrl_delete.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				IStructuredSelection selection = (IStructuredSelection)statesTableViewer.getSelection();

				if (selection.size() > 0) {
					actionMgr.doAction(IActionManager.REMOVE_MANY, workProduct, UmaPackage.eINSTANCE
							.getMethodElement_OwnedRules(), selection.toList(), -1);
				}
				updateControls();
			}
		});
		
		ctrl_name.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				updateControls();
			}			
		});
		
		ctrl_name.addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				if (e.keyCode == SWT.CR) {
					createState();
					updateControls();
				}				
			}			
		});
		
		statesTableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				updateControls();
			}			
		});
	}
	
	private void createState() {
		String stateName = ctrl_name.getText();
		
		if ((stateName != null) && (stateName.length() > 0)) {
			WorkProductPropUtil.getWorkProductPropUtil(actionMgr).getState(workProduct, stateName, true);
			ctrl_name.setText(""); //$NON-NLS-1$
		}
	}
	
	private void loadData() {
		statesTableViewer.setInput(workProduct);
	}
	
	private void updateControls() {
		String stateName = ctrl_name.getText();
		if ((stateName != null) && (stateName.length() > 0)) {
			ctrl_add.setEnabled(true);
		} else {
			ctrl_add.setEnabled(false);
		}
		
		IStructuredSelection selection = (IStructuredSelection)statesTableViewer.getSelection();		
		if (selection.size() > 0) {
			ctrl_delete.setEnabled(true);
		} else {
			ctrl_delete.setEnabled(false);
		}
	}

}
