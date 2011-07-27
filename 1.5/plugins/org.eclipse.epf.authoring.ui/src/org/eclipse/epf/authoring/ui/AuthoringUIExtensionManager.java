package org.eclipse.epf.authoring.ui;

import org.eclipse.epf.common.utils.ExtensionHelper;
import org.eclipse.epf.library.configuration.ConfigurationData;

public class AuthoringUIExtensionManager {

	private static AuthoringUIExtensionManager instance;

	public AuthoringUIExtensionManager() {
	}

	public static AuthoringUIExtensionManager getInstance() {
		if (instance == null) {
			Object obj = ExtensionHelper.create(
					AuthoringUIExtensionManager.class, null);
			if (obj instanceof AuthoringUIExtensionManager) {
				instance = (AuthoringUIExtensionManager) obj;
			} else {

				instance = new AuthoringUIExtensionManager();
			}
		}
		return instance;
	}
}
