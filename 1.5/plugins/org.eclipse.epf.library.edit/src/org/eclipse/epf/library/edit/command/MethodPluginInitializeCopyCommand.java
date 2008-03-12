package org.eclipse.epf.library.edit.command;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.command.CopyCommand.Helper;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.epf.library.edit.navigator.PluginUIPackagesItemProvider;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.edit.command.MethodElementInitializeCopyCommand;
import org.eclipse.epf.uma.edit.domain.TraceableAdapterFactoryEditingDomain;

public class MethodPluginInitializeCopyCommand extends
		MethodElementInitializeCopyCommand {


	/**
	 * Creates a new instance.
	 * 
	 * @param domain
	 *            the editing domain
	 * @param owner
	 *            the object being copied
	 * @param copyHelper
	 *            a helper class that is used to keep track of copied objects
	 *            and their associated copies
	 */
	public MethodPluginInitializeCopyCommand(EditingDomain domain,
			EObject owner, Helper copyHelper) {
		super(domain, owner, copyHelper);
	}

	
	/**
	 * @see org.eclipse.emf.edit.command.InitializeCopyCommand#doExecute()
	 */
	public void doExecute() {
		super.doExecute();

		// change name for the copy
		if (copy instanceof MethodElement) {
			MethodElement e = ((MethodElement) copy);
			if (e instanceof MethodPlugin) {
				Object parent = ((TraceableAdapterFactoryEditingDomain)domain).getSelectedObjectToCopy();
				if (parent instanceof PluginUIPackagesItemProvider) {
					Object parentParent = ((PluginUIPackagesItemProvider)parent).getParent();
					if (parentParent instanceof PluginUIPackagesItemProvider) {
						String deltaName = PluginUIPackagesItemProvider.getNameDelta((PluginUIPackagesItemProvider)parentParent, e);
						e.setName(deltaName);
					}
				}
			}
		}
	}
}
