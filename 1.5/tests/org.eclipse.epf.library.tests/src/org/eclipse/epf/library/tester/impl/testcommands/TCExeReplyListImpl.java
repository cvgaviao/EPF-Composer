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
import org.eclipse.epf.library.tester.iface.TCExeReplyList;

/**
 * Implementing CExeReplyList
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */

public class TCExeReplyListImpl implements TCExeReplyList {
	List replyList = null;
	String 	summaryReply;
	
	public int size() {
		return replyList == null ? 0 : replyList.size();
	}

	public TCExeReply item(int i) {
		return (TCExeReply) replyList.get(i);
	}
	
	public boolean passing() {	
		for (int i=0; i<size(); i++) {
			if (! item(i).passing()) {
				return false;
			}
		}		
		return true;
	}
	
	public void add(TCExeReply reply) {
		if (replyList == null) {
			replyList = new ArrayList();
		}
		replyList.add(reply);
	}
	
	public void setSummaryReply(String str) {
		summaryReply = str;
	}
	
	public String getSummaryReply() {
		return summaryReply;
	}

}
