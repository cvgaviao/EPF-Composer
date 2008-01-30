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
package org.eclipse.epf.tests.ui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.epf.library.edit.command.IUserInteractionHandler;
import org.eclipse.epf.library.edit.command.UserInput;
import org.eclipse.epf.library.edit.command.IUserInteractionHandler.IMessenger;
import org.eclipse.epf.library.edit.util.ExtensionManager;
import org.eclipse.epf.library.edit.util.ItemLabelProvider;
import org.eclipse.epf.library.tests.TestsPlugin;
import org.eclipse.jface.dialogs.ErrorDialog;

/**
 * @author Phong Nguyen Le - Oct 27, 2006
 * @since 1.0
 */
public class UserInteractionHandlerTest extends TestCase {

	/**
	 * Test method for
	 * {@link org.eclipse.epf.authoring.ui.actions.UserInteractionHandler#select(java.util.List, org.eclipse.jface.viewers.ILabelProvider, boolean, java.util.List, java.lang.String, java.lang.String)}.
	 */
	public void testSelect() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for
	 * {@link org.eclipse.epf.authoring.ui.actions.UserInteractionHandler#selectOne(int[], java.lang.String, java.lang.String, org.eclipse.core.runtime.IStatus)}.
	 */
	public void testSelectOne() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for
	 * {@link org.eclipse.epf.authoring.ui.actions.UserInteractionHandler#requestInput(java.lang.String, java.lang.String, java.util.List)}.
	 */
	public void testRequestInput() {
		List inputs = new ArrayList();
		for(int i=0; i<2; i++){
			List list = new ArrayList();
			list.add("Item1");
			list.add("Item2");
			UserInput input = new UserInput("Label Name"+i, i, true, list,
							new ItemLabelProvider(), null, null);
			inputs.add(input);
		}
		
		IUserInteractionHandler uiHandler = ExtensionManager
		.getDefaultUserInteractionHandler();
		uiHandler.requestInput("Testing Title", "Nice message", inputs);
		
		for(Iterator it = inputs.iterator(); it.hasNext();){
			Object ox = it.next();
			System.out.println("print : "+ ((UserInput)ox).getInput());
		}
		//fail("Not yet implemented");
	}

	public void testMessenger() throws Exception {
		IUserInteractionHandler uiHandler = ExtensionManager
				.getDefaultUserInteractionHandler();

		IMessenger messenger = uiHandler.getMessenger();

		// We are testing message dialogs but ErrorDialog.AUTOMATED_MODE is set
		// to true by Eclipse while running JUnit test. So we set this flag
		// back to false.
		//
		ErrorDialog.AUTOMATED_MODE = false;

		// test showError(String title, String msg);
		//
		messenger.showError("title", "message");
		messenger.showError(null, "NULL title");
		messenger.showError(null, null);

		// test showError(String title, String msg, IStatus status)
		//
		messenger.showError("title", "NUL status", null);
		String pluginID = TestsPlugin.PLUGIN_ID;
		messenger.showError("title", "message", new Status(IStatus.OK,
				pluginID, 0, "OK status", null));
		messenger.showError("title", "message", new Status(IStatus.ERROR,
				pluginID, 0, "ERROR status", new RuntimeException(
						"runtime exception")));
		MultiStatus status = new MultiStatus(pluginID, 0, "", null);
		status.add(new Status(IStatus.OK, pluginID, 0, "OK status", null));
		status.add(new Status(IStatus.ERROR, pluginID, 0, "ERROR status",
				new RuntimeException("runtime exception")));
		messenger.showError("title", "message", status);
	}
}
