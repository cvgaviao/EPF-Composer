package org.eclipse.epf.authoring.ui.providers;

import org.eclipse.epf.authoring.ui.forms.AssociationFormPage;
import org.eclipse.epf.uma.Task;

public class FormPageProviderExtender {

	private AssociationFormPage formPage;

	public FormPageProviderExtender(AssociationFormPage formPage) {
		this.formPage = formPage;
	}
	
	public boolean isActive(int providerIx) {
		return false;
	}
	
	public void dispose() {
		formPage = null;
	}
	
	public String getColumnText(Object object, int columnIndex, int providerIx) {
		return getFormPage().getLableProvider(providerIx).getColumnText(object, columnIndex);
	}
	
	public String getText(Object object, int providerIx) {
		return getFormPage().getLableProvider(providerIx).getText(object);
	}
	
	public Object[] getElements(Object object, int providerIx) {
		return getFormPage().getContentProvider(providerIx).getElements(object);
	}
	
	protected AssociationFormPage getFormPage() {
		return formPage;
	}
}
