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
package org.eclipse.epf.authoring.ui.views;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.List;

import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.library.edit.configuration.PracticeSubgroupItemProvider;
import org.eclipse.epf.library.edit.process.ActivityWrapperItemProvider;
import org.eclipse.epf.library.layout.BrowsingLayoutSettings;
import org.eclipse.epf.library.layout.ElementLayoutManager;
import org.eclipse.epf.library.layout.HtmlBuilder;
import org.eclipse.epf.library.layout.IElementLayout;
import org.eclipse.epf.library.layout.elements.AbstractProcessElementLayout;
import org.eclipse.epf.library.layout.elements.SummaryPageLayout;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.library.util.ResourceHelper;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.LocationAdapter;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.OpenWindowListener;
import org.eclipse.swt.browser.StatusTextEvent;
import org.eclipse.swt.browser.StatusTextListener;
import org.eclipse.swt.browser.WindowEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;


/**
 * Displays the HTML representation of a Method element in a SWT Browser.
 * 
 * @author Jinhua Xi
 * @author Kelvin Low
 * @since 1.0
 */
public class ElementHTMLViewer {

//	// The default HTML wrappers for disabling the "Display Tree Browser" link
//	// in the HTML content pages.
//	private static final String DEFAULT_CONTENT_WRAPPER_START = "<html><head> <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head><title>Rational Unfied Process</title>" //$NON-NLS-1$
//			+ "<script language=\"javascript\">function init() { parent.ory_toc=\"disable\"; parent.contentframe.location=\""; //$NON-NLS-1$
//
//	private static final String DEFAULT_CONTENT_WRAPPER_END = "\"; }</script></head>" //$NON-NLS-1$
//			+ "<frameset rows=\"100%,*\" frameborder=\"no\" border=\"0\" framespacing=\"0\" id=\"contentframeset\" name=\"contentframeset\">" //$NON-NLS-1$
//			+ "<frame src=\"about:blank\" id=\"contentframe\" name=\"contentframe\" scrolling=\"auto\"/>" //$NON-NLS-1$
//			+ "<frame src=\"about:blank\" id=\"blankframe\" name=\"blankframe\" onload=\"init()\"/>" //$NON-NLS-1$
//			+ "</frameset></html>"; //$NON-NLS-1$

//	// The default HTML wrappers for printing the process content pages.
//	private static final String DEFAULT_PRINT_WRAPPER_START = DEFAULT_CONTENT_WRAPPER_START
//			+ "print();"; //$NON-NLS-1$
//
//	private static final String DEFAULT_PRINT_WRAPPER_END = DEFAULT_CONTENT_WRAPPER_END;
//
//	private static final String PRINT_FILE = "print.html"; //$NON-NLS-1$

	// The HTML wrappers for disabling the "Display Tree Browser" link in the
	// HTML content pages.
//	private String contentWrapperSart;
//
//	private String contentWrapperEnd;

//	// The HTML wrappers for printing the HTML content pages.
//	private String printWrapperSart;
//
//	private String printWrapperEnd;
//
//	// The built-in browser for printing the HTML pages.
//	// don't use a second 
//	private Browser printBrowser;

	protected Browser browser = null;

	protected Composite parent;

	private String currentLocation = null;

	// save the current text from the status listener
	// we need this to open browser with this url 
	// since under linux browser can't be opened automatically, which is really bad
	private String currentText = null;
	private HtmlBuilder htmlBuilder;

	private Cursor waitCursor = null;

	/**
	 * Creates a new instance.
	 */
	public ElementHTMLViewer(Composite parent) {
		this.parent = parent;
		htmlBuilder = new HtmlBuilder();
		createControl();
	}

	/**
	 * Creates the SWT browser controls for viewing and printing the HTML
	 * content pages.
	 */
	private void createControl() {
		try {
			browser = new Browser(parent, SWT.NONE);
			browser.setLayoutData(new GridData(GridData.FILL_BOTH|GridData.GRAB_VERTICAL));
			browser.addLocationListener(new LocationAdapter() {
				public void changed(LocationEvent e) {
					//System.out.println("location changed");
				}

				public void changing(LocationEvent e) {
					//System.out.println("location changing...");
					// Get the current location.
					String location = getElementUrl(e.location);
					if ( location == null || location.startsWith(ResourceHelper.URL_STR_JAVASCRIPT))
					{
						return;
					}
					if (isLocationChanged(location)) {
						// Save the current location for printing purposes.
						currentLocation = location;
						generateHtml(currentLocation);
					}
				}
			});

			browser.addStatusTextListener(new StatusTextListener()
			{
				public void changed(StatusTextEvent event)
				{
					// Get the current text.
					String text = event.text;
					if (text != null && text.length() > 0)
					{
						// Save the current text for application launching purposes.
						currentText = text;
						//System.out.println("Status changed: " + text);
					}
				}
			});
			
			browser.addOpenWindowListener(new OpenWindowListener(){

				public void open(WindowEvent event) {
					// handle open window.
					// with mozilla in linux, open window does not work
					//System.out.println("Open window event: " + event);
					
					// windows can launch the browser without problem
					// linux needs more care
					if (SWT.getPlatform().equals("win32")) //$NON-NLS-1$
					{
						return;
					}
					
					Display.getCurrent().asyncExec(new Runnable()
					{
					    public void run()
						{
							try
							{
//								// if the url is a resource with relative path, find the absolute path
//								if ( currentText.startsWith("file:") )
//								{
//									// get the file path from the 
//								}
																
//								// open another swt browser since we have no (simple) way of creating a system default browser
//								// the Eclipse guys hide all the good apis as internal
								Shell shell = new Shell();
								GridLayout layout = new GridLayout();
								layout.marginHeight = 0;
								layout.marginWidth = 0;
								shell.setLayout(layout);
								Browser popup_browser = new Browser(shell, SWT.NONE);
								popup_browser.setLayoutData(new GridData(GridData.FILL_BOTH|GridData.GRAB_HORIZONTAL|GridData.GRAB_VERTICAL));
								popup_browser.setUrl(currentText);
																
								shell.open();

							}
							catch (Exception e)
							{
								e.printStackTrace();
							}
						}
					});
				}});
			
//			// don't use a seperate browser for printing since this will lose the 
//			// states of the current page, for example, the expanded sections, etc.
//			// Jinhua Xi,  3/5/2007
//			// Create the hidden browser used for printing the HTML content
//			// pages.
//			printBrowser = new Browser(parent, SWT.NONE);
//			printBrowser.setSize(1, 1);
//			printBrowser.setVisible(false);
//
//			// Linux: There is unused space in preview/browsing, which is in grey.
//			// this is needed for linux
//			GridData gd = new GridData();
//			gd.heightHint = 1;
//			printBrowser.setLayoutData(gd);
			
			
			// Read the HTML wrappers for disabling the "Display Tree Browser"
			// link in the HTML content pages.
//			contentWrapperSart = HTMLWrappers
//					.getString("CONTENT_WRAPPER_START"); //$NON-NLS-1$
//			if (contentWrapperSart == null) {
//				contentWrapperSart = DEFAULT_CONTENT_WRAPPER_START;
//			}
//			contentWrapperEnd = HTMLWrappers.getString("CONTENT_WRAPPER_END"); //$NON-NLS-1$
//			if (contentWrapperEnd == null) {
//				contentWrapperEnd = DEFAULT_CONTENT_WRAPPER_START;
//			}

//			// Read the HTML wrappers for printing the HTML content pages.
//			printWrapperSart = HTMLWrappers.getString("PRINT_WRAPPER_START"); //$NON-NLS-1$
//			if (printWrapperSart == null) {
//				printWrapperSart = DEFAULT_PRINT_WRAPPER_START;
//			}
//			printWrapperEnd = HTMLWrappers.getString("PRINT_WRAPPER_END"); //$NON-NLS-1$
//			if (printWrapperEnd == null) {
//				printWrapperEnd = DEFAULT_PRINT_WRAPPER_END;
//			}

			waitCursor = parent.getShell().getDisplay().getSystemCursor(
					SWT.CURSOR_WAIT);
		} catch (Exception e) {
			browser = null;
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
		}
	}

	private boolean isLocationChanged(String location) {
		if (location == null || currentLocation == null) {
			return false;
		}

		if (File.separatorChar != '/') {
			location = location.replace(File.separatorChar, '/');
			String oldLocation = getUrl(currentLocation);

			// Some times the current URL is a full path, so just compare the end.
			
			//  bug in IE6, the location in the location change event might be truncated if the url is too long, 
			// could be up to 159 characters.
			
			return oldLocation.indexOf(location) != 0;
		} else {
			return !location.endsWith(currentLocation)
					&& !currentLocation.endsWith(location);
		}

	}

	/**
	 * Returns the SWT browser that displays the HTML content pages.
	 */
	public Browser getBrowser() {
		return browser;
	}

	/**
	 * Sets focus to this control.
	 */
	public void setFocus() {
		if (browser != null) {
			browser.setFocus();
		}
	}

	/**
	 * Returns the HTML Builder.
	 */
	public HtmlBuilder getHtmlBuilder() {
		
		if ( htmlBuilder != null ) {
			BrowsingLayoutSettings.INSTANCE.update(htmlBuilder);
		}
		return htmlBuilder;
	}

	/**
	 * Sets the HTML Builder.
	 */
	public void setHtmlBuilder(HtmlBuilder builder) {
		this.htmlBuilder = builder;
	}

	/**
	 * Sets the Element Layout Manager.
	 */
	public void setLayoutManager(ElementLayoutManager layoutMgr) {
		htmlBuilder.setLayoutManager(layoutMgr);
	}


	
	/**
	 * Displays the HTML representation for a Method element.
	 */
	public void showElementContent(Object raw_element) {

		startWait();

		try {
			IElementLayout layout = null;
			String file_url = "about:blank"; //$NON-NLS-1$
			Object element = LibraryUtil.unwrap(raw_element);
			if ( raw_element instanceof ActivityWrapperItemProvider ) {
				ActivityWrapperItemProvider wrapper = (ActivityWrapperItemProvider)raw_element;
				Object proc = wrapper.getTopItem();
				if ( element instanceof MethodElement && proc instanceof org.eclipse.epf.uma.Process ) {
					String path = AbstractProcessElementLayout.getPath(wrapper);
					//System.out.println(topItem);
					layout = getHtmlBuilder().getLayoutManager()
						.createLayout((MethodElement)element, 
								(org.eclipse.epf.uma.Process)proc, path);
					file_url = getHtmlBuilder().generateHtml(layout);
				}
			} else if (raw_element instanceof PracticeSubgroupItemProvider) {
				PracticeSubgroupItemProvider provider = (PracticeSubgroupItemProvider) raw_element;
				layout = new SummaryPageLayout(getHtmlBuilder().getLayoutManager(),
						provider.getPractice(), provider.getText(null),
						provider.getText(null), (List) provider.getChildren(null),
						provider.getText(null));
				file_url = getHtmlBuilder().generateHtml(layout);
			} else if (element instanceof MethodElement) {
					file_url = getHtmlBuilder().generateHtml((MethodElement)element);
			}
			
			if ( file_url == null ) {
				file_url = "about:blank"; //$NON-NLS-1$
			}
			// on linux, the file path need to be specified as file, otherwise it will be treated as url
			// and casuign encoding/decoding issue
			// Linux: Configuration names containing accented characters cannot be browsed.
			else {			
				if (!SWT.getPlatform().equals("win32") && !file_url.startsWith("file://") && //$NON-NLS-1$ //$NON-NLS-2$
					!file_url.equals("about:blank")) //$NON-NLS-1$
				{
					file_url = "file://" + file_url; //$NON-NLS-1$
				}
				
				// Bug 201335 - Refresh does not work correctly for process pages in browsing mode
				// need to append the query string
				if ( layout instanceof AbstractProcessElementLayout ) {
					file_url += ((AbstractProcessElementLayout)layout).getQueryString();
				}
			}
		
			// Set the current location to avoid re-generating the HTML file in
			// respond to a location change.
			currentLocation = file_url;
			browser.setUrl(file_url);
		} catch (RuntimeException e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
		}

		endWait();
	}

	private void generateHtml(String url) {
		startWait();
		try {
			getHtmlBuilder().generateHtml(url);
		} catch (RuntimeException e) {
			AuthoringUIPlugin.getDefault().getLogger().logError(e);
		}

		endWait();
	}

	/**
	 * Returns the element URL.
	 */
	private String getElementUrl(String location) {
		if (location == null || location.length() == 0
				|| location.equals("about:blank")  //$NON-NLS-1$
				|| location.toLowerCase().startsWith(ResourceHelper.URL_STR_JAVASCRIPT)) {
			return null;
		}

		location = getUrl(location);
		if (location.indexOf(ResourceHelper.MISSING_PAGES_FOLDER) >= 0) {
			return null;
		}

		// make sure the url is an element url, check the guid
		if ( ResourceHelper.getGuidFromFileName(location) == null )
		{
			return null;
		}
		return location;
	}

	/**
	 * Returns the URL for the given location.
	 */
	private String getUrl(String location) {
		String publish_dir = getHtmlBuilder().getPublishDir();
		String url = fixPath(location);
		int index = url.toLowerCase().indexOf(
				fixPath(publish_dir).toLowerCase());
		if (index >= 0) {
			url = url.substring(index + publish_dir.length());
		}
		index = url.indexOf("#"); //$NON-NLS-1$
		if (index > 0) {
			url = url.substring(0, index);
		}
		return url;
	}

	private String fixPath(String path) {
		try {
			String platform = SWT.getPlatform();
			if ("win32".equals(platform) || "wpf".equals(platform)) { //$NON-NLS-1$ $NON-NLS-2$
			// org.eclipse.swt.browser.IE
				path = URLDecoder.decode(path, "ISO-8859-1"); //$NON-NLS-1$
			} else {
				// org.eclipse.swt.browser.Mozilla
				path = URLDecoder.decode(path, "UTF-8"); //$NON-NLS-1$
			}
			if (File.separatorChar != '/') {
				return path.replace(File.separatorChar, '/');
			}
			return path;
		} catch (UnsupportedEncodingException e) {
			return ""; //$NON-NLS-1$
		}
	}

	/**
	 * Prints the current HTML page.
	 */
	public void print() {
		boolean b = this.browser.execute("window.print();"); //$NON-NLS-1$
//		if ( !b ) {
//			final String printFile = htmlBuilder.getPublishDir() + PRINT_FILE;
//			if (currentLocation != null) {
//				StringBuffer wrapperHTML = new StringBuffer();
//				wrapperHTML.append(printWrapperSart).append(currentLocation)
//						.append(printWrapperEnd);
//				String htmlText = wrapperHTML.toString();
//				if (FileUtil.writeUTF8File(printFile, htmlText)) {
//					printBrowser.setUrl(printFile);
//				}
//			}
//		}
	}

	private void startWait() {
		parent.getShell().setCursor(waitCursor);
	}

	private void endWait() {
		parent.getShell().setCursor(null);
	}

	/**
	 * Refreshes the content.
	 * <p>
	 * Regenerates the content before refreshing the browser.
	 * 
	 */
	public void refresh() {
		generateHtml(currentLocation);
		browser.refresh();
	}

}
