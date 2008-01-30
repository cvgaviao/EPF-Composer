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

import java.io.File;

import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.tester.OutputDiffAnalyzor;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.library.tester.impl.TestCommandMgr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCCompareToGoldenFile extends TestCommandImpl {					
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(AT_GoldenFileName, element.getAttribute(AT_GoldenFileName));
		setAttribute(AT_DiffCount, element.getAttribute(AT_DiffCount));
		setAttribute(AT_ElementCount, element.getAttribute(AT_ElementCount));
	}	
	
	public TCExeReply execute() {
		String goldenFileName = getAttribute(AT_GoldenFileName);
		int diffCount = parseInteger(getAttribute(AT_DiffCount), 0);
		int elementCount = parseInteger(getAttribute(AT_ElementCount), 0);	
		TestCommandMgr owner = getOwner();
		
		Document outputDocument = owner.getOutputDocument();
		if (outputDocument == null) {
			return null;
		}
		
		String outputPath = owner.getOutputPath();
		try {
			XMLUtil.saveDocument(outputDocument, outputPath);
		} catch (Exception e){
			e.printStackTrace();
		}
		String path = owner.getTestFile().getAbsolutePath();
		int ix = path.lastIndexOf(".");
		path = path.substring(0, ix) + ".xml";
		File goldenOutput = new File(path);
		if (goldenOutput.exists()) {
			OutputDiffAnalyzor analysor = new OutputDiffAnalyzor(owner, null, outputDocument,
					goldenOutput, new File(outputPath));
			analysor.compare();
			if (analysor.getDiffCount() != diffCount ||
					elementCount != 0 && analysor.getElemComparedCount() != elementCount) {				
				String msg = "Ouptut and golden file comparison fails! ";
				owner.trace(msg);
				return new TCExeReplyImpl(msg, false);
			}
		}
		
		return null;
	}
}
