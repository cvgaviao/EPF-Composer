package org.eclipse.epf.toolbox.utils;

import java.io.File;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

public class XsltTransform {
	
	private static int i = 1;
	
	private static String f0 = "C:\\ec\\a-Learing\\Xslt\\f" + i + ".xml";
	private static String f1 = "C:\\ec\\a-Learing\\Xslt\\f" + i + ".xsl";
	private static String f2 = "C:\\ec\\a-Learing\\Xslt\\f" + i + ".html";

    public static void main(String[] args) throws Exception {
        StreamSource source = new StreamSource(new File(f0));
        StreamSource style = new StreamSource(new File(f1));
        StreamResult out = new StreamResult(new File(f2));

        TransformerFactory factory = TransformerFactory.newInstance();
        Transformer t = factory.newTransformer(style);
        t.transform(source, out);
    }

	    
}
