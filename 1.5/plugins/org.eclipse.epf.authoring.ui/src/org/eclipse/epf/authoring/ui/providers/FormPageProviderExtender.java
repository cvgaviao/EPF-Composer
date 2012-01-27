package org.eclipse.epf.authoring.ui.providers;

import org.eclipse.epf.authoring.ui.forms.AssociationFormPage;

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
		return formPage.getLableProvider(providerIx).getColumnText(object, columnIndex);
	}
	
	public String getText(Object object, int providerIx) {
		return formPage.getLableProvider(providerIx).getText(object);
	}
	
	
}
