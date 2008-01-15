package org.eclipse.epf.common.ui;

import org.eclipse.epf.common.AbstractActivator;
import org.eclipse.epf.common.IMessageCallback;
import org.eclipse.epf.common.ui.util.MsgDialog;

/**
 * message callback implementation
 * 
 * @author Jinhua Xi
 * @since 1.5
 *
 */
public class MessageCallback implements IMessageCallback {

	public void displayWarning(AbstractActivator plugin, String title, String msg, String reason) {
		displayWarning(plugin, title, msg, reason, null, null);		
	}
	
	public void displayWarning(AbstractActivator plugin, String msg, String reason, Throwable ex) {
		displayWarning(plugin, msg, reason, null, ex);
	}

	public void displayWarning(AbstractActivator plugin, String msg, String reason,
			String details, Throwable ex) {
		String title = CommonUIPlugin.getDefault().getWorkbench().getDisplay().getActiveShell().getText();
		displayWarning(plugin, title, msg, reason, details, ex);
	}
	
	public void displayWarning(AbstractActivator plugin, String title, String msg, String reason,
			String details, Throwable ex) {
		MsgDialog dlg = CommonUIPlugin.getDefault().getMsgDialog(plugin);
		dlg.displayWarning(title, msg, reason, details, ex);
	}
	
	public void displayError(AbstractActivator plugin, String title, String msg) {
		displayError(plugin, title, msg, null, null, null);
	}
	
	public void displayError(AbstractActivator plugin, String title, String msg, Throwable ex) {
		displayError(plugin, title, msg, null, null, ex);
	}
	public void displayError(AbstractActivator plugin, String title, String msg, String reason, String details, Throwable ex) {
		MsgDialog dlg = CommonUIPlugin.getDefault().getMsgDialog(plugin);
		dlg.displayError(title, msg, reason, details, ex);

	}

}
