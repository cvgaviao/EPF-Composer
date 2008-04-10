package org.eclipse.epf.authoring.ui.providers;

import java.util.Map;

import org.eclipse.epf.authoring.ui.forms.ConfigViewPage;
import org.eclipse.epf.authoring.ui.forms.ConfigurationDescription;
import org.eclipse.epf.authoring.ui.forms.ConfigurationPage;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.ui.forms.editor.FormEditor;

public class ConfigurationEditorDefaultPageProvider implements
		IMethodElementEditorPageProviderExtension {

	public Map<Object, String> getPages(Map<Object, String> pageMap,
			FormEditor editor, Object input) {
		if (input instanceof MethodConfiguration) {
			pageMap.put(new ConfigurationDescription(editor), null);
			pageMap.put(new ConfigurationPage(editor), null);
			pageMap.put(new ConfigViewPage(editor), null);
		}
		return pageMap;
	}

}
