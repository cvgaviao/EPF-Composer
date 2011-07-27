package org.eclipse.epf.authoring.ui.views;

import org.eclipse.epf.authoring.ui.actions.LibraryActionBarContributor;
import org.eclipse.epf.authoring.ui.views.LibraryView.LibraryViewActionBarContributor;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.ISelection;

public class LibraryViewExtender {
	
	private LibraryView libraryView;
	private ActionBarExtender actionBarExtender;

	public LibraryViewExtender(LibraryView libraryView) {
		this.libraryView = libraryView;
	}
	
	public ActionBarExtender getActionBarExtender() {
		if (actionBarExtender == null) {
			actionBarExtender = newActionBarExtender();
		}		
		return actionBarExtender;
	}
	
	protected ActionBarExtender newActionBarExtender() {
		return new ActionBarExtender();
	}
	
	public static class ActionBarExtender {
		private LibraryActionBarContributor actionBar;

		public ActionBarExtender() {			
		}
		
		
		public void menuAboutToShow(IMenuManager menuManager) {			
		}
		
		public void updateSelection(ISelection selection) {				
		}
		
		public LibraryActionBarContributor getActionBar() {
			return actionBar;
		}


		public void setActionBar(LibraryActionBarContributor actionBar) {
			this.actionBar = actionBar;
		}
		
	}
	
	
}
