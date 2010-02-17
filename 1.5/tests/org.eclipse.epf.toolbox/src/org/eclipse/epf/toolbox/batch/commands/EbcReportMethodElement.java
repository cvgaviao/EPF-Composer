package org.eclipse.epf.toolbox.batch.commands;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.toolbox.batch.EbcReportMgr;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

public class EbcReportMethodElement extends EpfBatchCommandImpl {

	EbcReportMgr reportMgr;
	
	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.ElementType, element.getAttribute(C.ElementType));
		setAttribute(C.Recursive, element.getAttribute(C.Recursive));
		setAttribute(C.Name, element.getAttribute(C.Name));
		
		reportMgr = getMgr().getReportMgr();
	}	
		
	protected void executeBody(final EbcExeReply reply) {
		boolean recursive = parseBoolean(C.Recursive, false);
		for (MethodElement elem: getReportedElements()) {
			report(elem, recursive);
		}		
	}

	private Collection<MethodElement> getReportedElements() {
		MethodLibrary lib = getMgr().getCurrentBaseLib();
		String elementType = getAttribute(C.ElementType);
		String name = getAttribute(C.Name);
		
		Set<MethodElement> reportedSet = new LinkedHashSet<MethodElement>(); 
		
		if (elementType.equals("MethodLibrary")) {
			reportedSet.add(lib);
		} else {
			boolean showAll = name.equals("*");
			
			for (Iterator it = lib.eAllContents(); it.hasNext();) {
				Object obj = it.next();
				if (obj instanceof MethodElement) {
					MethodElement me = (MethodElement) obj;
					if (showAll || me.getName().equals(name)) {
						if (me.getClass().getName().contains(elementType)) {
							reportedSet.add(me);
						}
					}
				}
			}
		}
		
		return reportedSet;
	}
	
	private void reportContents(List contents) {
		if (contents == null) {
			return;
		}
		for (int i = 0; i < contents.size(); i++) {
			report(contents.get(i), true);
		}
	}
	

	
	private void report(Object obj, boolean recursive) {
		reportMgr.report(obj);
		if (recursive) {
			if (obj instanceof EObject) {
				reportContents(((EObject) obj).eContents());
			}
		}
	}
	
	
}
