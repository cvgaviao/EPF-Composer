//------------------------------------------------------------------------------
// Copyright (c) 2004, 2005 IBM Corporation.  All Rights Reserved.
//------------------------------------------------------------------------------
package org.eclipse.epf.library.edit.process.command;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.epf.library.edit.LibraryEditPlugin;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.Concept;
import org.eclipse.epf.uma.EstimationConsiderations;
import org.eclipse.epf.uma.Example;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.Guideline;
import org.eclipse.epf.uma.Report;
import org.eclipse.epf.uma.ReusableAsset;
import org.eclipse.epf.uma.Roadmap;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.Template;
import org.eclipse.epf.uma.ToolMentor;

/**
 * Command to add guidance to breakdown element/activity
 *  
 * @author Shilpa Toraskar
 * @since 1.5
 */
public class AddGuidanceToBreakdownElementCommand extends AddMethodElementCommand {

	private List<Guidance> guidances;

	private BreakdownElement brElement;

	private Collection modifiedResources;

	public AddGuidanceToBreakdownElementCommand(BreakdownElement brElement, List<Guidance> guidances) {

		super(TngUtil.getOwningProcess(brElement));

		this.guidances = guidances;
		this.brElement = brElement;

		this.modifiedResources = new HashSet();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#execute()
	 */
	public void execute() {

		// add to default configuration if not there already
		if (!super.addToDefaultConfiguration(guidances))
			return;

		redo();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#redo()
	 */
	public void redo() {

		if (!guidances.isEmpty()) {
			for (Iterator it = guidances.iterator(); it.hasNext();) {
				Object obj = it.next();
				if (obj instanceof Guidance) {
					Guidance item = (Guidance) obj;

					// guidances for breakdown element
					if (item instanceof Checklist) {
						brElement.getChecklists().add((Checklist) item);
					} else if (item instanceof Concept) {
						brElement.getConcepts().add((Concept) item);
					} else if (item instanceof Example) {
						brElement.getExamples().add((Example) item);
					} else if (item instanceof SupportingMaterial) {
						brElement.getSupportingMaterials().add((SupportingMaterial) item);
					} else if (item instanceof Guideline) {
						brElement.getGuidelines().add((Guideline) item);
					} else if (item instanceof ReusableAsset) {
						brElement.getReusableAssets().add((ReusableAsset) item);
					} else if (item instanceof Template) {
						brElement.getTemplates().add((Template) item);
					} else if (item instanceof Report) {
						brElement.getReports().add((Report) item);
					} else if (item instanceof EstimationConsiderations) {
						brElement.getEstimationconsiderations().add((EstimationConsiderations) item);
					} else if (item instanceof ToolMentor) {
						brElement.getToolmentor().add((ToolMentor) item);
					} else if (item instanceof Roadmap) {
						if (brElement instanceof Activity) {
							((Activity) brElement).getRoadmaps().add((Roadmap) item);
						}
					} else {
						LibraryEditPlugin.getDefault().getLogger()
								.logError("Cant set guidance " + item.getType().getName() + ":" + item.getName()); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#undo()
	 */
	public void undo() {
		if (!guidances.isEmpty()) {
			// basically remove from configuration if anything was added
			super.undo();

			for (Iterator it = guidances.iterator(); it.hasNext();) {
				Object obj = it.next();
				if (obj instanceof Guidance) {
					Guidance item = (Guidance) obj;

					// guidances for content element
					if (item instanceof Checklist) {
						brElement.getChecklists().remove(item);
					} else if (item instanceof Concept) {
						brElement.getConcepts().remove(item);
					} else if (item instanceof Example) {
						brElement.getExamples().remove(item);
					} else if (item instanceof SupportingMaterial) {
						brElement.getSupportingMaterials().remove(item);
					} else if (item instanceof Guideline) {
						brElement.getGuidelines().remove(item);
					} else if (item instanceof ReusableAsset) {
						brElement.getReusableAssets().remove(item);
					} else if (item instanceof Template) {
						brElement.getTemplates().remove((Template) item);
					} else if (item instanceof Report) {
						brElement.getReports().remove((Report) item);
					} else if (item instanceof EstimationConsiderations) {
						brElement.getEstimationconsiderations().remove((EstimationConsiderations) item);
					} else if (item instanceof ToolMentor) {
						brElement.getToolmentor().remove((ToolMentor) item);
					} else if (item instanceof Roadmap) {
						if (brElement instanceof Activity) {
							((Activity) brElement).getRoadmaps().remove((Roadmap) item);
						}
					} else {
						LibraryEditPlugin.getDefault().getLogger()
								.logError("Cant set guidance " + item.getType().getName() + ":" + item.getName()); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		}
	}

	protected boolean prepare() {
		return true;
	}

	public Collection getModifiedResources() {
		if (!guidances.isEmpty() ) {
			if (brElement.eResource() != null) {
				modifiedResources.add(brElement.eResource());
			}
		}
		return modifiedResources;
	}

	public Collection getAffectedObjects() {
		if (brElement != null) {
			return Arrays.asList(new Object[] { brElement });
		}
		return super.getAffectedObjects();
	}
}
