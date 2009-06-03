package org.eclipse.epf.authoring.ui.views;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.epf.library.configuration.ConfigurationFilter;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.jface.viewers.Viewer;

public class ConfigurationViewFilter extends ConfigurationFilter {

	protected Viewer viewer;
	
	public ConfigurationViewFilter(MethodConfiguration methodConfig, Viewer viewer) {
		super(methodConfig, false);
		this.viewer = viewer;
	}
	
	private boolean isValidViewer() {
		return viewer != null &&  viewer.getControl() != null && !viewer.getControl().isDisposed();
	}
	
	public void notifyChanged(final Notification msg) {
//		if (viewer == null) {
//			return;
//		}
		if (!isValidViewer()) {
			return;
		}
	
		
		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				switch (msg.getEventType()) {
				case Notification.ADD:
				case Notification.ADD_MANY:
				case Notification.REMOVE:
				case Notification.REMOVE_MANY:
					refreshViewer();
				}
			}
		});
	}
	
	public void refreshViewer() {
		viewer.refresh();
	}
	
}
