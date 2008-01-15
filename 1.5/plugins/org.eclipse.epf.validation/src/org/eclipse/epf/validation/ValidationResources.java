package org.eclipse.epf.validation;

import org.eclipse.osgi.util.NLS;

public class ValidationResources extends NLS {
	private static final String BUNDLE_NAME = "org.eclipse.epf.validation.Resources"; //$NON-NLS-1$

	public static String circularDependency_error;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, ValidationResources.class);
	}

	private ValidationResources() {
	}
}
