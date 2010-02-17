package org.eclipse.epf.toolbox.batch.commands;

import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.toolbox.batch.EbcBatchCommandMgr;
import org.w3c.dom.Element;


public interface EpfBatchCommand {

	void setMgr(EbcBatchCommandMgr mgr);
	void setAttribute(String attName, String attValue);
	String getAttribute(String attName);
	void parse(Element element);
	EbcExeReply execute();
	
}
