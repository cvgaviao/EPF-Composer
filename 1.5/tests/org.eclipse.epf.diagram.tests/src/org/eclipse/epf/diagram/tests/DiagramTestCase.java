package org.eclipse.epf.diagram.tests;

import junit.framework.TestCase;

import org.eclipse.epf.authoring.ui.AuthoringUIPlugin;
import org.eclipse.epf.library.tests.LibraryTestHelper;
import org.eclipse.epf.uma.MethodLibrary;

/**
* The abstract base class for all Diagram JUnit test cases.
* 
* @author Shilpa Toraskar
* @since 1.2
*/
public abstract class DiagramTestCase extends TestCase {

	protected MethodLibrary library;

	protected boolean deleteLibraryOnExit = false;

	public DiagramTestCase(String name) {
		super(name);
	}

	
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
		LibraryTestHelper.createTestMethodLibrary();
		
		// activate the Authoring plugins to initialize something
		// the reason to put it after the library is created is to avoid the Open Library dialog
		AuthoringUIPlugin.getDefault();

	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
		
		LibraryTestHelper.closeLibrary();
	}

}
