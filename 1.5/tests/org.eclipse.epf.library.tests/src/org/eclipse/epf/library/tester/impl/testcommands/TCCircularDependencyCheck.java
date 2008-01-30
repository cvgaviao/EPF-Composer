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

import org.eclipse.epf.library.edit.validation.DependencyInfoMgr;
import org.eclipse.epf.library.edit.validation.Tracer;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCCircularDependencyCheck extends TestCommandImpl {					
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_ErrorCount, element.getAttribute(AT_ErrorCount));
		setAttribute(AT_CircularElementCount, element.getAttribute(AT_CircularElementCount));
	}	
	
	public TCExeReply execute() {
		int errorCount = parseInteger(getAttribute(AT_ErrorCount), 0);
		int circularElementCount = parseInteger(getAttribute(AT_CircularElementCount), 0);
		final TestCommandMgr owner = getOwner();
		
		Tracer tracer = new Tracer() {
			public void trace(String line) {
				owner.trace(line);
			}
		};
		
		DependencyInfoMgr infoMgr = new DependencyInfoMgr(owner.getCurrentBaseLib());
		DependencyInfoMgr.CheckResult checkResult = infoMgr.checkCircularDependnecy(tracer);
		
		if (checkResult.getErrorCount() != errorCount || 
				checkResult.getCircularElementCount() != circularElementCount) {
			String msg = "checkResult.getErrorCount() = " + checkResult.getErrorCount() + "\n";
			msg += "checkResult.getCircularElementCount() = " + checkResult.getCircularElementCount();
			owner.trace(msg);
			return new TCExeReplyImpl(msg, false);
		}
		
		return null;
	}
}
