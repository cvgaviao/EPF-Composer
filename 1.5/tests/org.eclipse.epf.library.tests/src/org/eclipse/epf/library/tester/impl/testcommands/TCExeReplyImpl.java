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
package org.eclipse.epf.library.tester.impl.testcommands;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.epf.library.tester.iface.TCExeReply;

/**
 * Implementing TCExeReply
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCExeReplyImpl implements TCExeReply {
	
	private boolean passing = true;
	private List msgs;
	
	public TCExeReplyImpl(String msg, boolean passing) {
		addMsg(msg);
		setPassing(passing);
	}
	
	public String getLastMsg() {
		int sz = msgs == null ? 0 : msgs.size();			
		return sz == 0 ? null : (String) msgs.get(sz - 1);
	}
	
	public List getMsgs() {
		return msgs;
	}
	
	public boolean passing() {
		return passing;
	}
	
	public void addMsg(String msg) {
		if (msg == null || msg.length() == 0) {
			return;
		}
		if (msgs == null) {
			msgs = new ArrayList();
		}
		msgs.add(msg);
	}
	
	public void setPassing(boolean b) {
		passing = b;
	}
}

