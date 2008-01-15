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
package org.eclipse.epf.search.analysis;

import java.io.IOException;
import java.util.ResourceBundle;
import java.util.Set;

import org.apache.lucene.analysis.StopAnalyzer;
import org.apache.lucene.analysis.StopFilter;
import org.apache.lucene.analysis.Token;
import org.apache.lucene.analysis.TokenFilter;
import org.apache.lucene.analysis.TokenStream;
import org.eclipse.epf.common.utils.StrUtil;

/**
 * A text filter that handles Unicode 4.1 characters.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public final class TextFilter extends TokenFilter {

	private static Set stopWords = null;

	/**
	 * Creates a new instance.
	 */
	public TextFilter(TokenStream in) {
		super(in);
		if (stopWords == null) {
			loadStopWords();
		}
	}

	/**
	 * @see org.apache.lucene.analysis.TokenStream#next()
	 */
	public final Token next() throws IOException {
		for (Token token = input.next(); token != null; token = input.next()) {
			String tokenText = token.termText();
			if (!stopWords.contains(tokenText)) {
				return token;
			}
		}
		return null;
	}

	/**
	 * Loads the stop words defined in the StopWords.properties file.
	 */
	private void loadStopWords() {
		String[] words = null;
		try {
			ResourceBundle bundle = ResourceBundle.getBundle(TextFilter.class
					.getPackage().getName()
					+ ".StopWords"); //$NON-NLS-1$				
			String property = bundle.getString("Search.stopWords"); //$NON-NLS-1$
			words = StrUtil.split(property, " ,"); //$NON-NLS-1$		
		} catch (Exception e) {
			words = StopAnalyzer.ENGLISH_STOP_WORDS;
		}
		stopWords = StopFilter.makeStopSet(words);
	}

}