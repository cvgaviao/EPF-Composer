/**
 * 
 */
package org.eclipse.epf.diagram.ui.actions;

import java.util.List;

import org.eclipse.gef.GraphicalViewer;
import org.eclipse.gmf.runtime.diagram.ui.actions.internal.ArrangeAction;
import org.eclipse.gmf.runtime.diagram.ui.editparts.DiagramEditPart;
import org.eclipse.gmf.runtime.diagram.ui.parts.DiagramCommandStack;
import org.eclipse.gmf.runtime.diagram.ui.parts.IDiagramEditDomain;
import org.eclipse.ui.IWorkbenchPage;

/**
 * @author skannoor
 *
 */
public class CustomArrangeAction extends ArrangeAction {

	
	private DiagramEditPart diagramEditPart;
	private GraphicalViewer viewer;

	/**
	 * @param workbenchPage
	 * @param selectionOnly
	 */
	public CustomArrangeAction(IWorkbenchPage workbenchPage,
			boolean selectionOnly, DiagramEditPart diagramEditPart, GraphicalViewer viewer) {
		super(workbenchPage, selectionOnly);
		this.diagramEditPart = diagramEditPart;
		this.viewer = viewer;
		// TODO Auto-generated constructor stub
	}

	@Override
	protected List getSelectedObjects() {
		return diagramEditPart.getChildren();
	}
	
	@Override
	protected DiagramEditPart getDiagramEditPart() {
		return diagramEditPart;
	}
	@Override
	protected DiagramCommandStack getDiagramCommandStack() {
		return new DiagramCommandStack((IDiagramEditDomain)viewer.getEditDomain());
	}
}
