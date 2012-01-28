package org.eclipse.epf.authoring.ui.providers;

import org.eclipse.epf.authoring.ui.forms.AssociationFormPage;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.DefaultElementRealizer;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.UmaPackage;

public class FormPageProviderExtender {

	private AssociationFormPage formPage;
	private boolean locked = false;
	protected static UmaPackage up = UmaPackage.eINSTANCE;

	public FormPageProviderExtender(AssociationFormPage formPage) {
		this.formPage = formPage;
	}
	
	public boolean isActive(int providerIx) {
		return false;		
	}
	
	private void setLocked(boolean locked) {
		this.locked = locked;
	}

	protected boolean isLocked() {
		return locked;
	}
	
	public void dispose() {
		formPage = null;
	}
	
	public String getColumnText(Object object, int columnIndex, int providerIx) {
		setLocked(true);
			try {
		return getFormPage().getLableProvider(providerIx).getColumnText(object, columnIndex);
		} finally {
			setLocked(false);
		}
	}
	
	public String getText(Object object, int providerIx) {
		setLocked(true);
		try {
			return getFormPage().getLableProvider(providerIx).getText(object);
		} finally {
			setLocked(false);
		}
	}
	
	public Object[] getElements(Object object, int providerIx) {
		setLocked(true);
		try {
			return getFormPage().getContentProvider(providerIx).getElements(object);
		} finally {
			setLocked(false);
		}
	}
	
	protected AssociationFormPage getFormPage() {
		return formPage;
	}
	
	protected MethodConfiguration getConfig() {
		return LibraryService.getInstance().getCurrentMethodConfiguration();
	}
	
	protected ElementRealizer getRealizer() {
		ElementRealizer realizer = DefaultElementRealizer
				.newElementRealizer(getConfig());
		return realizer;
	}
	
	
}
