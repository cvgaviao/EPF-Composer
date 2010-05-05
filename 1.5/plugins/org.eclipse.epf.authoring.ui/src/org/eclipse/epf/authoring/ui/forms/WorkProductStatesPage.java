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
	private Table ctrl_states;
	private TableViewer statesTableViewer;
	private IStructuredContentProvider statesViewerContentProvider;
	private ITableLabelProvider statesViewerLabelProvider;
	private Button ctrl_add, ctrl_delete;
	private Text ctrl_name;
	private Constraint currentState;
	
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
		statesComposite.setLayout(new GridLayout(4, false));
		statesSection.setClient(statesComposite);
		
		createStatesArea(statesComposite);
		
		toolkit.paintBordersFor(statesComposite);
		
		addListeners();
		loadData();
		updateControls();
	}
	
	protected void createStatesArea(Composite parent) {
		Composite pane1 = toolkit.createComposite(parent);
		{
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.horizontalSpan = 3;
			pane1.setLayoutData(gridData);
			pane1.setLayout(new GridLayout());
		}
		
		Label l_name = toolkit.createLabel(pane1, AuthoringUIText.STATES_TEXT);
		{
			GridData gridData = new GridData(GridData.BEGINNING);
			l_name.setLayoutData(gridData);
		}
		
		ctrl_states = toolkit.createTable(pane1, SWT.MULTI);
		{
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.heightHint = 150;
			ctrl_states.setLayoutData(gridData);
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

		Composite pane2 = toolkit.createComposite(parent);
		pane2.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_CENTER	| GridData.HORIZONTAL_ALIGN_CENTER));
		pane2.setLayout(new GridLayout());
		
		ctrl_add = toolkit.createButton(pane2, AuthoringUIText.ADD_BUTTON_TEXT, SWT.NONE);
		ctrl_add.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		ctrl_delete = toolkit.createButton(pane2, AuthoringUIText.DELETE_BUTTON_TEXT, SWT.NONE);
		ctrl_delete.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Label nameLabel = toolkit.createLabel(parent, AuthoringUIText.STATES_NAME_TEXT);
		{
			GridData gridData = new GridData(GridData.BEGINNING);
			gridData.horizontalSpan = 4;
			nameLabel.setLayoutData(gridData);
		}
		
		ctrl_name = toolkit.createText(parent, "", SWT.SINGLE); //$NON-NLS-1$
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			gridData.horizontalSpan = 4;
			ctrl_name.setLayoutData(gridData);
		}		
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
				String stateName = getNextAvailableStateName();
				WorkProductPropUtil.getWorkProductPropUtil(actionMgr).getState(workProduct, stateName, true);
				updateControls();
			}
		});
		
		ctrl_delete.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				IStructuredSelection selection = (IStructuredSelection)statesTableViewer.getSelection();
				UmaPackage up = UmaPackage.eINSTANCE;
				actionMgr.doAction(IActionManager.REMOVE_MANY, workProduct, up
						.getMethodElement_OwnedRules(), selection.toList(), -1);
				currentState = null;
				ctrl_name.setText(""); //$NON-NLS-1$
				updateControls();
			}
		});
		
		statesTableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection)statesTableViewer.getSelection();
				if (selection.size() == 1) {
					currentState = (Constraint)selection.getFirstElement();
					ctrl_name.setText(currentState.getBody());
				}
				updateControls();
			}			
		});
		
		ctrl_name.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				String newName = ctrl_name.getText();
				UmaPackage up = UmaPackage.eINSTANCE;
				actionMgr.doAction(IActionManager.SET, currentState, up
						.getConstraint_Body(), newName, -1);
				updateControls();
			}			
		});
	}
	
	private String getNextAvailableStateName() {
		Set<Constraint> states = WorkProductPropUtil.getWorkProductPropUtil(actionMgr).getAllStates(workProduct);
		String baseName = AuthoringUIText.STATES_DEFAULT_NAME_TEXT;
		int flag = 1;
		
		String name = baseName + " " + flag; //$NON-NLS-1$
		while(isNameTaken(states, name)) {
			flag++;
			name = baseName + " " + flag; //$NON-NLS-1$
		}

		return name;
	}
	
	private boolean isNameTaken(Set<Constraint> states, String name) {
		for (Constraint state : states) {
			if (state.getBody().equals(name)) {
				return true;
			}
		}
		
		return false;
	}
	
	private void loadData() {
		statesTableViewer.setInput(workProduct);
	}
	
	private void updateControls() {
		IStructuredSelection selection = (IStructuredSelection)statesTableViewer.getSelection();
		
		if (selection.size() > 0) {
			ctrl_delete.setEnabled(true);
		} else {
			ctrl_delete.setEnabled(false);
		}
		
		if (currentState != null) {
			ctrl_name.setEditable(true);
		} else {
			ctrl_name.setEditable(false);
		}
	}

}
