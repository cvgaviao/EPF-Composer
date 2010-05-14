/*******************************************************************************
* Licensed Materials - Property of IBM
* (c) Copyright IBM Corporation 2007,2009. All Rights Reserved.
*
* Note to U.S. Government Users Restricted Rights:
* Use, duplication or disclosure restricted by GSA ADP Schedule
* Contract with IBM Corp. 
*******************************************************************************/

package org.eclipse.epf.authoring.ui.forms;

import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.util.UIHelper;
import org.eclipse.epf.library.edit.TngAdapterFactory;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.library.edit.util.MethodPluginPropUtil;
import org.eclipse.epf.library.edit.util.WorkProductPropUtil;
import org.eclipse.epf.uma.Constraint;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableFontProvider;
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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
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
	private MethodPlugin activePlugin;
	private IActionManager actionMgr;
	
	private Section statesSection;
	private Composite statesComposite;
	
	private Text ctrl_name;
	private Button ctrl_add;
	
	private Table ctrl_wp_states;
	private TableViewer wpStatesViewer;
	private IStructuredContentProvider wpStatesViewerContentProvider;
	private ITableLabelProvider wpStatesViewerLabelProvider;
	
	private Button ctrl_assign, ctrl_unassign;
	
	private Table ctrl_global_states;
	private TableViewer globalStatesViewer;
	private IStructuredContentProvider globalStatesViewerContentProvider;
	private ITableLabelProvider globalStatesViewerLabelProvider;
	
	private Button ctrl_delete;

	public WorkProductStatesPage(FormEditor editor) {
		super(editor, FORM_PAGE_ID, AuthoringUIText.WORK_PRODUCT_STATES_PAGE_TITLE);
	}
	
	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		workProduct = (WorkProduct) contentElement;
		activePlugin = UmaUtil.getMethodPlugin(workProduct);
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
		//the first row
		toolkit.createLabel(parent, null);
		toolkit.createLabel(parent, null);
		Composite nameComposite = toolkit.createComposite(parent);
		{
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
			gridData.horizontalSpan = 2;
			nameComposite.setLayoutData(gridData);
		}
		nameComposite.setLayout(new GridLayout(3, false));		
		toolkit.createLabel(nameComposite, AuthoringUIText.STATES_NAME_TEXT);		
		ctrl_name = toolkit.createText(nameComposite, null, SWT.SINGLE);
		ctrl_name.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		ctrl_add = toolkit.createButton(nameComposite, AuthoringUIText.STATES_ADD_TEXT, SWT.NONE);
		{
			GridData gridData = new GridData();
			gridData.widthHint = 80;
			ctrl_add.setLayoutData(gridData);
		}
		
		//the second row
		Composite wpStateComposite = toolkit.createComposite(parent);
		wpStateComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		wpStateComposite.setLayout(new GridLayout());
		toolkit.createLabel(wpStateComposite, AuthoringUIText.STATES_WP_STATE_TEXT);
		ctrl_wp_states = toolkit.createTable(wpStateComposite, SWT.MULTI);
		{
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.widthHint = 300;
			gridData.heightHint = 150;
			ctrl_wp_states.setLayoutData(gridData);
		}		
		
		Composite btnComposite1 = toolkit.createComposite(parent);
		btnComposite1.setLayoutData(new GridData(GridData.FILL_BOTH));
		btnComposite1.setLayout(new GridLayout());
		toolkit.createLabel(btnComposite1, null);
		toolkit.createLabel(btnComposite1, null);
		toolkit.createLabel(btnComposite1, null);
		toolkit.createLabel(btnComposite1, null);
		ctrl_assign = toolkit.createButton(btnComposite1, AuthoringUIText.STATES_ASSIGN_TEXT, SWT.NONE);
		{
			GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_CENTER |GridData.GRAB_HORIZONTAL);
			gridData.widthHint = 80;
			ctrl_assign.setLayoutData(gridData);
		}
		ctrl_unassign = toolkit.createButton(btnComposite1, AuthoringUIText.STATES_UNASSIGN_TEXT, SWT.NONE);
		{
			GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_CENTER |GridData.GRAB_HORIZONTAL);
			gridData.widthHint = 80;
			ctrl_unassign.setLayoutData(gridData);
		}
		
		Composite globalStateComposite = toolkit.createComposite(parent);
		globalStateComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		globalStateComposite.setLayout(new GridLayout());
		toolkit.createLabel(globalStateComposite, AuthoringUIText.STATES_GLOBAL_STATE_TEXT);
		ctrl_global_states = toolkit.createTable(globalStateComposite, SWT.MULTI);
		{
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.widthHint = 300;
			gridData.heightHint = 150;
			ctrl_global_states.setLayoutData(gridData);
		}
		
		Composite btnComposite2 = toolkit.createComposite(parent);
		btnComposite2.setLayoutData(new GridData(GridData.FILL_BOTH));
		btnComposite2.setLayout(new GridLayout());
		toolkit.createLabel(btnComposite2, null);
		ctrl_delete = toolkit.createButton(btnComposite2, AuthoringUIText.STATES_DELETE_TEXT, SWT.NONE);
		{
			GridData gridData = new GridData();
			gridData.widthHint = 80;
			ctrl_delete.setLayoutData(gridData);
		}
		
		//the third row
		toolkit.createLabel(parent, null);
		toolkit.createLabel(parent, null);
		toolkit.createLabel(parent, AuthoringUIText.STATES_GLOBAL_NOTES_TEXT);

		initProviders();
		
		wpStatesViewer = new TableViewer(ctrl_wp_states);
		wpStatesViewer.setContentProvider(wpStatesViewerContentProvider);
		wpStatesViewer.setLabelProvider(wpStatesViewerLabelProvider);
		wpStatesViewer.setComparator(new ViewerComparator() {
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof Constraint) && (e2 instanceof Constraint)) {
					String name1 = ((Constraint)e1).getBody();
					String name2 = ((Constraint)e2).getBody();					
					return getComparator().compare(name1, name2);
				}
				
				return 0;
			}
		});
		
		globalStatesViewer = new TableViewer(ctrl_global_states);
		globalStatesViewer.setContentProvider(globalStatesViewerContentProvider);
		globalStatesViewer.setLabelProvider(globalStatesViewerLabelProvider);
		globalStatesViewer.setComparator(new ViewerComparator() {
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof Constraint) && (e2 instanceof Constraint)) {
					Constraint state1 = (Constraint)e1;
					Constraint state2 = (Constraint)e2;
					if ((isLocalState(state1)) && (!isLocalState(state2))) {
						return -1;
					} else if ((!isLocalState(state1)) && (isLocalState(state2))) {
						return 1;
					} else {
						String name1 = state1.getBody();
						String name2 = state2.getBody();					
						return getComparator().compare(name1, name2);
					}					
				}
				
				return 0;
			}
		});
		
		toolkit.paintBordersFor(nameComposite);
		toolkit.paintBordersFor(wpStateComposite);
		toolkit.paintBordersFor(btnComposite1);
		toolkit.paintBordersFor(globalStateComposite);
		toolkit.paintBordersFor(btnComposite2);
	}
	
	private void initProviders() {
		wpStatesViewerContentProvider = new AdapterFactoryContentProvider(
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

		wpStatesViewerLabelProvider = new AdapterFactoryLabelProvider(
				TngAdapterFactory.INSTANCE.getNavigatorView_ComposedAdapterFactory()) {
			public String getColumnText(Object element, int columnIndex) {
				if (element instanceof Constraint) {
					return ((Constraint)element).getBody();
				}
				
				return null;
			}
		};
		
		globalStatesViewerContentProvider = new AdapterFactoryContentProvider(
				TngAdapterFactory.INSTANCE.getNavigatorView_ComposedAdapterFactory()) {
			public Object[] getElements(Object object) {
				if (object instanceof WorkProduct) {
					List<Constraint> globalStates = MethodPluginPropUtil.getMethodPluginPropUtil(actionMgr)
							.getWorkProductStatesInLibrary(activePlugin);
					return globalStates.toArray();
				}
				
				return new Object[0];
			}
		};

		globalStatesViewerLabelProvider = new GlobalStatesLabelProvider(
				TngAdapterFactory.INSTANCE.getNavigatorView_ComposedAdapterFactory());
	}
	
	private void addListeners() {
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
		
		ctrl_add.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				createState();
				updateControls();
			}
		});
		
		wpStatesViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				updateControls();
			}			
		});
		
		ctrl_assign.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				assignState();
				updateControls();
			}
		});
		
		ctrl_unassign.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				unassignState();
				updateControls();
			}
		});
		
		globalStatesViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				updateControls();
			}			
		});
		
		ctrl_delete.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				deleteState();
				updateControls();
			}
		});		
	}
	
	private void createState() {
		String stateName = ctrl_name.getText();
		
		if ((stateName != null) && (stateName.length() > 0)) {			
			MethodPluginPropUtil.getMethodPluginPropUtil(actionMgr).getWorkProductState(
					activePlugin, stateName, true);
			ctrl_name.setText(""); //$NON-NLS-1$
		}
	}
	
	private void deleteState() {
		IStructuredSelection selection = (IStructuredSelection)globalStatesViewer.getSelection();
		
		for (Object obj : selection.toList()) {
			if (obj instanceof Constraint) {
				MethodPluginPropUtil.getMethodPluginPropUtil(actionMgr).removeWorkProductState(
						activePlugin, ((Constraint)obj).getBody());
			}
		}
	}
	
	private void assignState() {
		IStructuredSelection selection = (IStructuredSelection)globalStatesViewer.getSelection();
		
		for (Object obj : selection.toList()) {
			if (obj instanceof Constraint) {
				WorkProductPropUtil.getWorkProductPropUtil(actionMgr).addWorkProductState(
						workProduct, ((Constraint)obj).getBody());				
			}
		}		
	}
	
	private void unassignState() {
		IStructuredSelection selection = (IStructuredSelection)wpStatesViewer.getSelection();
		
		for (Object obj : selection.toList()) {
			if (obj instanceof Constraint) {
				WorkProductPropUtil.getWorkProductPropUtil(actionMgr).removeWorkProductState(
						workProduct, ((Constraint)obj).getBody());				
			}
		}		
	}
	
	private void loadData() {
		wpStatesViewer.setInput(workProduct);
		globalStatesViewer.setInput(workProduct);
	}
	
	private void updateControls() {
		String stateName = ctrl_name.getText();
		if ((stateName != null) && (stateName.length() > 0)) {
			ctrl_add.setEnabled(true);
		} else {
			ctrl_add.setEnabled(false);
		}
		
		IStructuredSelection globalSelection = (IStructuredSelection)globalStatesViewer.getSelection();
		if (globalSelection.size() > 0) {
			ctrl_assign.setEnabled(true);			
		} else {
			ctrl_assign.setEnabled(false);			
		}		
		if (canDelete(globalSelection)) {
			ctrl_delete.setEnabled(true);
		} else {
			ctrl_delete.setEnabled(false);
		}
		
		IStructuredSelection wpSelection = (IStructuredSelection)wpStatesViewer.getSelection();		
		if (wpSelection.size() > 0) {
			ctrl_unassign.setEnabled(true);
		} else {
			ctrl_unassign.setEnabled(false);
		}
		
		wpStatesViewer.refresh();
		globalStatesViewer.refresh();
	}
	
	private boolean canDelete(IStructuredSelection selection) {
		if (selection.size() > 0) {
			for (Object obj : selection.toList()) {
				if (obj instanceof Constraint) {
					if (!isLocalState((Constraint)obj)) {
						return false;
					}
				}				
			}			
			return true;		
		}		
		
		return false;
	}
	
    private boolean isLocalState(Constraint state) {
    	List<Constraint> allLocalStates = MethodPluginPropUtil.getMethodPluginPropUtil(actionMgr)
    			.getWorkProductStatesInPlugin(activePlugin);
    	
    	return allLocalStates.contains(state);		    	
    }
	
	public class GlobalStatesLabelProvider extends AdapterFactoryLabelProvider implements ITableFontProvider {
		private FontRegistry registry = new FontRegistry();
		private Font systemFont;
		
		public GlobalStatesLabelProvider(AdapterFactory adapterFactory) {
			super(adapterFactory);
		}
		
		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof Constraint) {
				return ((Constraint)element).getBody();
			}
			
			return null;
		}
		
	    public Font getFont(Object element, int columnIndex) {
	    	if (systemFont == null) {
	    		systemFont = Display.getCurrent().getSystemFont();
	    	}
	    	
	    	if ((element instanceof Constraint) && (isLocalState((Constraint)element))) {
	    		return registry.getBold(systemFont.getFontData()[0].getName());
	    	}

	    	return systemFont;
	    }	
	}

}
