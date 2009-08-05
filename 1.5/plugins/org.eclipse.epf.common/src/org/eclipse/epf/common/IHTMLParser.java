package org.eclipse.epf.common;

import java.io.File;
import java.util.Properties;

public interface IHTMLParser {

	public String getText();
	
	public String getSummary();
	
	public Properties getMetaTags();
	
	public void parse(File file) throws Exception;
}
