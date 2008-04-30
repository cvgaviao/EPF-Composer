package org.eclipse.epf.library.layout;

import java.io.UnsupportedEncodingException;

import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;

/**
 * Special content validator for the Rich Text Editor
 * @author Jeff Hardy
 *
 */
public class RichTextContentValidator extends DefaultContentValidator {

	/**
	 * validates a URL without decoding it
	 */
	public LinkInfo validateLink(MethodElement owner, String attributes,
			String text, MethodConfiguration config, String tag) {

		LinkInfo info = new LinkInfo(owner, this, pubDir, tag) {
			protected String decode(String str) throws UnsupportedEncodingException {
				return str;
			}
		};
		info.validateLink(attributes, text, config);

		return info;
	}

}
