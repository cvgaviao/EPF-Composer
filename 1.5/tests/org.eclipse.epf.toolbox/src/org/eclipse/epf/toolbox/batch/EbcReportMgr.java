package org.eclipse.epf.toolbox.batch;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.epf.common.utils.XMLUtil;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.toolbox.ToolboxPlugin;
import org.eclipse.epf.uma.MethodElement;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class EbcReportMgr {

	private EbcBatchCommandMgr ebcMgr;
	private File reportFile;
	private Document reportDocument;
	private Set<Object> repoptObjectSet;	
	private static String umaPackagePath = "org.eclipse.epf.uma.impl";
	private Element rootElement;
	
	public EbcReportMgr(EbcBatchCommandMgr ebcMgr) {
		this.ebcMgr = ebcMgr;
		getReportDocument();
	}

	public void setReportFile(File reportFile) {
		this.reportFile = reportFile;
	}
	
	public void saveDocument() {
		try {
			XMLUtil.saveDocument(getReportDocument(), reportFile.getAbsolutePath());
		} catch (Exception e){
			ToolboxPlugin.getDefault().getLogger().logError(e);
		}
	}

	public Document getReportDocument() {
		if (reportDocument == null) {			
			try {
				reportDocument = XMLUtil.createDocument();
				rootElement = reportDocument.createElement("Report");
		        reportDocument.appendChild(rootElement);
		        repoptObjectSet = new HashSet<Object>();
			} catch (Exception e) {
				ToolboxPlugin.getDefault().getLogger().logError(e);
			}
		}
		return reportDocument;
	}			
	
	public void report(Object obj) {
		if (repoptObjectSet.contains(obj)) {
			return;
		}
		report_(obj);
		repoptObjectSet.add(obj);
	}
	
	private void report_(Object obj) {
		Document doc = getReportDocument();
		
		String tag = null;
		
		String clsName = obj.getClass().getName();
		if (clsName.startsWith(umaPackagePath)) {
			tag = clsName.substring(25);
			if (tag.endsWith("Impl")) {
				int ix = tag.length() - 4;
				tag = tag.substring(0, ix);
			}
		}
		
		Resource res = null;
		MethodElement me = null;
		if (obj instanceof MethodElement) {
			me = (MethodElement) obj;
			res = me.eResource();
		}		
		
		String path = res == null ? "?" : res.getURI().toFileString();
		
		Element elem = doc.createElement(tag);
		elem.setAttribute("path", path);
		 
		String textContent = obj.toString();
		int ix = textContent.indexOf("(name:");
		if (ix > 0) {
			textContent = textContent.substring(ix);
		}
		
		elem.setTextContent(textContent);
		
		if (me != null) {
			List features = LibraryUtil.getStructuralFeatures(me);

			if (features != null) {
				for (int i = 0; i < features.size(); i++) {
					EStructuralFeature feature = (EStructuralFeature) features
							.get(i);
					if (feature instanceof EReference) {
						EReference ref = (EReference) feature;
						if (!ref.isContainment()) {
							Object value = me.eGet(ref);
							if (value != null) {
								if (value instanceof List) {
									for (Object subValue: (List) value) {
										addRefElem(doc, elem, ref, subValue);
									}
								} else {
									addRefElem(doc, elem, ref, value);
								}
							}
						}
					}
				}
			}
		}		
		rootElement.appendChild(elem);		
	}

	private void addRefElem(Document doc, Element elem, EReference ref, Object value) {
		Element refElem = doc.createElement(ref
				.getName());
		elem.appendChild(refElem);
		String str = value.toString();
		if (value instanceof MethodElement) {
			MethodElement me = (MethodElement) value;
			str = me.getName() + ", " + me.getGuid();
		}
		refElem.setTextContent(str);
	}	
	
}
