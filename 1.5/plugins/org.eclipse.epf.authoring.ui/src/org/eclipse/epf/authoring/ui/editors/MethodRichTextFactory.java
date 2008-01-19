package org.eclipse.epf.authoring.ui.editors;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichText;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichTextEditor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorSite;

public final class MethodRichTextFactory {
	
	private static final String methodRichTextExtensionPointName = "org.eclipse.epf.authoring.ui.methodRichText"; //$NON-NLS-1$
	private static final String methodRichTextEditorExtensionPointName = "org.eclipse.epf.authoring.ui.methodRichTextEditor"; //$NON-NLS-1$
	
	private static IConfigurationElement methodRichTextExtension = null;
	private static IConfigurationElement methodRichTextEditorExtension = null;
	
	public static IMethodRichText createMethodRichText(Composite parent, int style, String basePath) {
		IMethodRichText methodRichText = getMethodRichText();
		if (methodRichText != null) {
			methodRichText.init(parent, style, basePath);
		}
		return methodRichText;
	}
	
	public static IMethodRichTextEditor createMethodRichTextEditor(Composite parent, int style, String basePath, IEditorSite editorSite, MethodElement methodElement, Label label) {
		IMethodRichTextEditor methodRichTextEditor = getMethodRichTextEditor();
		if (methodRichTextEditor != null) {
			methodRichTextEditor.init(parent, style, basePath, editorSite, methodElement, label);
		}
		return methodRichTextEditor;
	}
	
	private static IMethodRichText getMethodRichText() {
		if (methodRichTextExtension == null) {
			IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
			IConfigurationElement[] extensions = extensionRegistry.getConfigurationElementsFor(methodRichTextExtensionPointName);

			int version = -1;
			
			for (int i=0; i < extensions.length; i++) {
				IConfigurationElement element = extensions[i];
				int otherVersion = parseVersion(element.getAttribute("version")); //$NON-NLS-1$
				if(otherVersion > version) {
					methodRichTextExtension = element;
					version = otherVersion;
				}
			}
		}
		
		try {
			return (IMethodRichText)methodRichTextExtension.createExecutableExtension("class");
		} catch (CoreException e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
			return null;
		}
	}
	
	private static IMethodRichTextEditor getMethodRichTextEditor() {
		if (methodRichTextEditorExtension == null) {
			IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
			IConfigurationElement[] extensions = extensionRegistry.getConfigurationElementsFor(methodRichTextEditorExtensionPointName);
	
			int version = -1;
			
			for (int i=0; i < extensions.length; i++) {
				IConfigurationElement element = extensions[i];
				int otherVersion = parseVersion(element.getAttribute("version")); //$NON-NLS-1$
				if(otherVersion > version) {
					methodRichTextEditorExtension = element;
					version = otherVersion;
				}
			}
		}
		try {
			if (isDebug()) {
				System.out.println("MethodRichTextFactory using " + methodRichTextEditorExtension.getAttribute("class"));
			}
			return (IMethodRichTextEditor)methodRichTextEditorExtension.createExecutableExtension("class");
		} catch (CoreException e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
			return null;
		}
	}

	
	public static int parseVersion(String str) {
		try {
			return Integer.parseInt(str);
		}
		catch(NumberFormatException e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
			return 0;
		}		
	}

	private static boolean isDebug() {
		return AuthoringUIPlugin.getDefault().isDebugging();
	}
	
}
