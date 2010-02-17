package org.eclipse.epf.toolbox.batch.commands;

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.toolbox.batch.C;
import org.eclipse.epf.toolbox.batch.EbcExeReply;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.w3c.dom.Element;

public class EbcOpenLibrary extends EpfBatchCommandImpl {

	public void parse(Element element) {
		super.parse(element);
		setAttribute(C.Path, element.getAttribute(C.Path));
	}	

	protected void executeBody(final EbcExeReply reply) {
		String path = getAttribute(C.Path);
		MethodLibrary lib = getMgr().loadBaseLib(path);
		
		if (localDebug) {
			Collection col = lib.eContents();
			for (Object obj : col) {
				System.out.println("LD> obj: " + obj);
			}
		}
		
		if (false) {
			for (Iterator it = lib.eAllContents(); it.hasNext();) {
				Object obj = it.next();
				if (obj instanceof MethodElement) {
					MethodElement me = (MethodElement) obj;
					String label = TngUtil.getLabelWithPath(me);
					getMgr().trace("LD> " + me.getClass().getName().substring(25) + ": " + label);
				}
			}

		}

	}
	
}
