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
package org.eclipse.epf.rcp.ui.wizards;

import java.util.List;

import org.eclipse.core.runtime.Path;
import org.eclipse.epf.ui.wizards.WizardCategories;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.dialogs.ImportWizard;
import org.eclipse.ui.internal.dialogs.WizardCollectionElement;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardElement;
import org.eclipse.ui.model.AdaptableList;
import org.eclipse.ui.wizards.IWizardCategory;

/**
 * UIImport class will filter out the wizard categories which are not related to
 * EPF. UIImport allows only UnCategorized
 * <link>WizardsRegistryReader.UNCATEGORIZED_WIZARD_CATEGORY</link> category (<link>org.eclipse.ui.Others</link>)
 * and filters as well in Others category wizards list.
 * 
 * @author Bingxue Xu
 * @author Shashidhar Kannoori
 * @autor Kelvin Low
 * @since 1.0
 */

public class UIImportWizard extends ImportWizard {

	/**
	 * Returns the import wizards that are available for invocation. Also filter
	 * import wizard list of uncategorized (org.eclipse.ui.Others)
	 */
	protected AdaptableList getAvailableImportWizards() {
		// TODO: imports are still flat - we need to get at the flat list. All
		// wizards will be in the "other" category.
		IWizardCategory root = WorkbenchPlugin.getDefault()
				.getImportWizardRegistry().getRootCategory();
		WizardCollectionElement category = (WizardCollectionElement) root
				.findCategory(new Path(WizardCategories.IMPORT_WIZARDS_CATEGORY));
		AdaptableList result;
		if (category == null)
			result = new AdaptableList();
		else
			result = category.getWizardAdaptableList();

		AdaptableList filteredResult = doFilter(result);
		if (filteredResult.size() > 0) {
			return filteredResult;
		}
		return result;
	}

	/**
	 * Filter out non-allowable import wizard categories
	 * 
	 * @param list
	 * @return AdaptableList
	 */
	public AdaptableList doFilter(AdaptableList list) {
		List extensions = UIImportWizardExtensionPoint.getInstance()
				.getPageProviders();
		AdaptableList filteredResult = new AdaptableList();
		Object[] objs = list.getChildren();
		for (int i = 0; i < objs.length; i++) {
			WorkbenchWizardElement obj = (WorkbenchWizardElement) objs[i];
			String id = obj.getId();
			if (!extensions.isEmpty() && extensions.contains(id)) {
				filteredResult.add(obj);
			}
		}
		return filteredResult;
	}
}
