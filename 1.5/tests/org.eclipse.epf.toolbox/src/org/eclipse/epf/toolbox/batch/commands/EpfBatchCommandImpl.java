package org.eclipse.epf.toolbox.batch.commands;

import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.Status;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.toolbox.batch.EbcBatchCommandMgr;
import org.w3c.dom.Element;

public class EpfBatchCommandImpl implements EpfBatchCommand {

	private boolean verbose = false;
	private Element element;
	protected static boolean localDebug = false;
	private EbcBatchCommandMgr mgr;
	private Map atttibuteMap = new LinkedHashMap();
	
	public void setMgr(EbcBatchCommandMgr mgr) {
		this.mgr = mgr;
	}
	
	protected Map getAttributeMap() {
		return atttibuteMap;
	}
	
	public void setAttribute(String attName, String attValue) {
		if (attValue != null && attValue.length() > 0) {
			atttibuteMap.put(attName, attValue);
		}
	}
	
	public String removeAttribute(String attName) {
		return (String) atttibuteMap.remove(attName);
	}
	
	public String getAttribute(String attName) {
		return (String) atttibuteMap.get(attName);
	}
	
	public void parse(Element element) {
		this.element = element;
		/*setAttribute(AT_Verbose, element.getAttribute(AT_Verbose));
		verbose = parseBoolean(AT_Verbose, verbose);*/
	}
	
	public void parseAtExecute() {
	}
	
	protected boolean getVerbose() {
		return verbose;
	}
	
	protected Element getElement() {
		return element;
	}
	
	protected void parseChildren(Element element) {
	}
	
	protected EbcBatchCommandMgr getMgr() {
		return mgr;
	}
	
	public EbcExeReply execute() {
		traceBeforeExcute();	
		final EbcExeReply reply = new EbcExeReply();				
		parseAtExecute();
		
		try {
			executeBody(reply);
		} catch (Exception e) {
			reply.addStatus(Status.ERROR, e.getMessage(), e);
		}

		traceAfterExcute(reply);		
		return reply;
	}
		
	protected void executeBody(final EbcExeReply reply) {
	}
	
	public String tagName() {
		return mgr == null ? null : mgr.getTagName(this);
	}
	
	protected boolean parseBoolean(String att, boolean defaultValue) {
		String toCheckStr = defaultValue ? "false" : "true";
		String str = getAttribute(att);
		if (str != null && str.equalsIgnoreCase(toCheckStr)) {
			return !defaultValue;
		}
		return defaultValue;
	}

	protected int parseInteger(String att, int defaultValue) {
		if (att != null && att.length() > 0) {
			return Integer.parseInt(att);
		}
		return defaultValue;
	}
	
	protected void traceBeforeExcute() {
		if (getMgr().isTrace()) {
			String id = getElement().getAttribute(C.Id);
			getMgr().trace("Begin " + getMgr().getTagName(this) + ".execute, id: " + id);
		}
	}
	
	protected void traceAfterExcute(EbcExeReply reply) {
		if (getMgr().isTrace()) {
			String id = getElement().getAttribute(C.Id);
			getMgr().trace("End   " + getMgr().getTagName(this) + ".execute, id: " + id + ", reply: " + reply);
			getMgr().trace("");
		}
	}
	
}
