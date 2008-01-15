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
package org.eclipse.epf.library.ui.wizards;

import java.io.File;

import org.eclipse.epf.common.ui.util.MsgBox;
import org.eclipse.epf.library.ILibraryService;
import org.eclipse.epf.library.edit.ui.UserInteractionHelper;
import org.eclipse.epf.library.layout.LayoutResources;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.ui.LibraryUIResources;
import org.eclipse.epf.library.ui.dialogs.LibraryBackupDialog;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Utility class to back up library.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class LibraryBackupUtil {

	private String path = null;

	/**
	 * Prompts to backup the current library
	 * 
	 * @param shell
	 */
	public static void promptBackupCurrentLibrary(Shell shell, ILibraryService service) {
		String libPathStr = service.getCurrentMethodLibraryLocation();
		File libPath = new File(libPathStr);
		new LibraryBackupUtil().doBackup(shell, libPath, service);
	}
	
	/**
	 * Prompts to backup library
	 * 
	 * @param shell
	 * @param libPath
	 */
	public static void promptBackupLibrary(Shell shell, File libPath) {
		new LibraryBackupUtil().doBackup(shell, libPath, null);
	}

	private void doBackup(final Shell shell, final File libPath, ILibraryService service) {
		path = null;

		SafeUpdateController.syncExec(new Runnable() {
			public void run() {
				Shell s = shell;
				if (s == null) {
					s = MsgBox.getDefaultShell();
				}

				if (s == null) {
					s = new Shell(MsgBox.getDisplay());
				}

				String title = LibraryUIResources.backupLibraryDialog_title;
				String message = LibraryUIResources.backupLibraryDialog_text;
				String backupPath = libPath.getAbsolutePath() + ".backup"; //$NON-NLS-1$	

				LibraryBackupDialog dlg = new LibraryBackupDialog(s, title,
						message, backupPath);

				if (dlg.open() == Dialog.OK) {
					path = dlg.getPath();
				}
			}
		});

		if (path != null) {
			if (service == null) {
				backup(libPath, new File(path));
			} else {
				service.getCurrentLibraryManager().backupMethodLibrary(path);
			}
		}

	}

	/**
	 * Back up library 
	 * 
	 * @param source
	 * @param dest
	 */
	public static void backup(final File source, final File dest) {
		Runnable runnable = new Runnable() {
			public void run() {
				try {
					// excude the non-library files that might be locked by rmc.
					// these files may cause backup to fail due to file lock.
					String excludes = ".lock"; //$NON-NLS-1$
					LayoutResources.copyDir(source, dest, "**", excludes); //$NON-NLS-1$
				} catch (RuntimeException e) {
					e.printStackTrace();
				}
			}
		};

		UserInteractionHelper.runWithProgress(runnable,
				LibraryUIResources.backingUpLibraryTask_name);
	}

}
