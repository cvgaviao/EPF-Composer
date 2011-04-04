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
package org.eclipse.epf.authoring.ui.forms;

import org.eclipse.epf.authoring.ui.AuthoringUIResources;
import org.eclipse.epf.authoring.ui.AuthoringUIText;
import org.eclipse.epf.authoring.ui.editors.MethodElementEditor;
import org.eclipse.epf.authoring.ui.richtext.IMethodRichText;
import org.eclipse.epf.library.edit.command.IActionManager;
import org.eclipse.epf.uma.Practice;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;


/**
 * The Description page in a Practice editor.
 * 
 * @author Shashidhar Kannoori
 * @author Kelvin Low
 * @since 1.0
 */
public class PracticeDescriptionPage extends GuidanceDescriptionPage {

	private IMethodRichText ctrl_additional_info, ctrl_problem,
			ctrl_background;

	private IMethodRichText ctrl_goals, ctrl_application, ctrl_levels_adoption;

	private Practice guidance;

	/**
	 * Creates a new instance.
	 */
	public PracticeDescriptionPage(FormEditor editor) {
		super(editor);
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.GuidanceDescriptionPage#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		guidance = (Practice) contentElement;
		publishPracticeOn = true;
		setContentFieldHeight(200);
		setFullDescOn(true);
		contentOn = false;
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.GuidanceDescriptionPage#createEditorContent(org.eclipse.ui.forms.widgets.FormToolkit)
	 */
	protected void createEditorContent(FormToolkit toolkit) {
		super.createEditorContent(toolkit);
		label_base.setText(AuthoringUIText.BASE_ELEMENT_TEXT);
	}
	
	@Override
	protected void createDetailSectionContent() {
		// Problem is named "Purpose" in the UI
		ctrl_problem = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIText.PURPOSE_TEXT, 40, 400,
				DETAIL_SECTION_ID);

		ctrl_goals = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIText.GOALS_TEXT, 40, 400,
				DETAIL_SECTION_ID);
		ctrl_background = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIText.BACKGROUND_TEXT, 40, 400,
				DETAIL_SECTION_ID);

		if (fullDescOn) {
			ctrl_full_desc = createRichTextEditWithLinkForSection(toolkit,
					detailComposite, AuthoringUIText.MAIN_DESCRIPTION_TEXT,
					100, 400, DETAIL_SECTION_ID);
		}
		
		// Application field is called "How to read this practice" in the UI
		ctrl_application = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIResources.practice_application_text, 40, 400,
				DETAIL_SECTION_ID);
		ctrl_levels_adoption = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIText.LEVEL_OF_ADOPTION_TEXT, 40,
				400, DETAIL_SECTION_ID);
		ctrl_additional_info = createRichTextEditWithLinkForSection(toolkit,
				detailComposite, AuthoringUIText.ADDITIONAL_INFO_TEXT, 40, 400,
				DETAIL_SECTION_ID);
	}

	/**
	 * Add listeners
	 *
	 * @see org.eclipse.epf.authoring.ui.forms.GuidanceDescriptionPage#addListeners()
	 */
	protected void addListeners() {
		super.addListeners();
		final MethodElementEditor editor = (MethodElementEditor) getEditor();

		final ModifyListener contentModifyListener = editor
				.createModifyListener(guidance.getPresentation());

		ctrl_additional_info
				.setModalObject((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation());
		ctrl_additional_info.setModalObjectFeature(UmaPackage.eINSTANCE
				.getPracticeDescription_AdditionalInfo());
		ctrl_additional_info.addModifyListener(contentModifyListener);
		ctrl_additional_info.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_additional_info;
				if (!control.getModified()) {
					return;
				}
				String oldContent = ((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation()).getAdditionalInfo();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						control, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							(org.eclipse.epf.uma.PracticeDescription) guidance
									.getPresentation(),
							UmaPackage.eINSTANCE
									.getPracticeDescription_AdditionalInfo(),
							newContent, -1);
					if (success && isVersionSectionOn()) {
						updateChangeDate();
					}
				}
			}
		});

		ctrl_application
				.setModalObject((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation());
		ctrl_application.setModalObjectFeature(UmaPackage.eINSTANCE
				.getPracticeDescription_Application());
		ctrl_application.addModifyListener(contentModifyListener);
		ctrl_application.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_application;
				if (!control.getModified()) {
					return;
				}
				String oldContent = ((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation()).getApplication();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						control, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							(org.eclipse.epf.uma.PracticeDescription) guidance
									.getPresentation(),
							UmaPackage.eINSTANCE
									.getPracticeDescription_Application(),
							newContent, -1);
					if (success && isVersionSectionOn()) {
						updateChangeDate();
					}
				}
			}
		});

		ctrl_goals.setModalObject((org.eclipse.epf.uma.PracticeDescription) guidance
				.getPresentation());
		ctrl_goals.setModalObjectFeature(UmaPackage.eINSTANCE
				.getPracticeDescription_Goals());
		ctrl_goals.addModifyListener(contentModifyListener);
		ctrl_goals.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_goals;
				if (!control.getModified()) {
					return;
				}
				String oldContent = ((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation()).getGoals();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						control, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = editor.getActionManager()
							.doAction(
									IActionManager.SET,
									(org.eclipse.epf.uma.PracticeDescription) guidance
											.getPresentation(),
									UmaPackage.eINSTANCE
											.getPracticeDescription_Goals(),
									newContent, -1);
					if (success && isVersionSectionOn()) {
						updateChangeDate();
					}
				}
			}
		});

		ctrl_problem.setModalObject((org.eclipse.epf.uma.PracticeDescription) guidance
				.getPresentation());
		ctrl_problem.setModalObjectFeature(UmaPackage.eINSTANCE
				.getPracticeDescription_Problem());
		ctrl_problem.addModifyListener(contentModifyListener);
		ctrl_problem.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_problem;
				String oldContent = ((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation()).getProblem();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						control, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							(org.eclipse.epf.uma.PracticeDescription) guidance
									.getPresentation(),
							UmaPackage.eINSTANCE
									.getPracticeDescription_Problem(),
							newContent, -1);
					if (success && isVersionSectionOn()) {
						updateChangeDate();
					}
				}
			}
		});

		ctrl_background
				.setModalObject((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation());
		ctrl_background.setModalObjectFeature(UmaPackage.eINSTANCE
				.getPracticeDescription_Background());
		ctrl_background.addModifyListener(contentModifyListener);
		ctrl_background.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_background;
				String oldContent = ((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation()).getBackground();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						control, oldContent)) {
					return;
				}
				String newContent = control.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							(org.eclipse.epf.uma.PracticeDescription) guidance
									.getPresentation(),
							UmaPackage.eINSTANCE
									.getPracticeDescription_Background(),
							newContent, -1);
					if (success && isVersionSectionOn()) {
						updateChangeDate();
					}
				}
			}
		});

		ctrl_levels_adoption
				.setModalObject((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation());
		ctrl_levels_adoption.setModalObjectFeature(UmaPackage.eINSTANCE
				.getPracticeDescription_LevelsOfAdoption());
		ctrl_levels_adoption.addModifyListener(contentModifyListener);
		ctrl_levels_adoption.addListener(SWT.Deactivate, new Listener() {
			public void handleEvent(Event e) {
				IMethodRichText control = descExpandFlag ? ctrl_expanded
						: ctrl_levels_adoption;
				String oldContent = ((org.eclipse.epf.uma.PracticeDescription) guidance
						.getPresentation()).getLevelsOfAdoption();
				if (((MethodElementEditor) getEditor()).mustRestoreValue(
						control, oldContent)) {
					return;
				}
				String newContent = descExpandFlag ? ctrl_expanded.getText()
						: ctrl_levels_adoption.getText();
				if (!newContent.equals(oldContent)) {
					boolean success = editor.getActionManager().doAction(
							IActionManager.SET,
							(org.eclipse.epf.uma.PracticeDescription) guidance
									.getPresentation(),
							UmaPackage.eINSTANCE
									.getPracticeDescription_LevelsOfAdoption(),
							newContent, -1);
					if (success && isVersionSectionOn()) {
						updateChangeDate();
					}
				}
			}
		});
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.GuidanceDescriptionPage#refresh(boolean)
	 */
	protected void refresh(boolean editable) {
		super.refresh(editable);
		ctrl_additional_info.setEditable(editable);
		ctrl_application.setEditable(editable);
		ctrl_goals.setEditable(editable);
//		ctrl_problem.setEditable(editable);
		ctrl_levels_adoption.setEditable(editable);
		ctrl_background.setEditable(editable);
	}

	/**
	 * Load initial data from model
	 *
	 * @see org.eclipse.epf.authoring.ui.forms.GuidanceDescriptionPage#loadData()
	 */
	protected void loadData() {
		super.loadData();
		if (guidance != null) {
			org.eclipse.epf.uma.PracticeDescription guidanceDescription = (org.eclipse.epf.uma.PracticeDescription) guidance
					.getPresentation();
			ctrl_additional_info
					.setText(guidanceDescription.getAdditionalInfo() == null ? "" : guidanceDescription.getAdditionalInfo()); //$NON-NLS-1$
			ctrl_problem
					.setText(guidanceDescription.getProblem() == null ? "" : guidanceDescription.getProblem()); //$NON-NLS-1$
			ctrl_background
					.setText(guidanceDescription.getBackground() == null ? "" : guidanceDescription.getBackground()); //$NON-NLS-1$
			ctrl_application
					.setText(guidanceDescription.getApplication() == null ? "" : guidanceDescription.getApplication()); //$NON-NLS-1$
			ctrl_goals
					.setText(guidanceDescription.getGoals() == null ? "" : guidanceDescription.getGoals()); //$NON-NLS-1$
			ctrl_levels_adoption
					.setText(guidanceDescription.getLevelsOfAdoption() == null ? "" : guidanceDescription.getLevelsOfAdoption()); //$NON-NLS-1$
		}
	}

	/**
	 * @see org.eclipse.epf.authoring.ui.forms.GuidanceDescriptionPage#getContentElement()
	 */
	protected Object getContentElement() {
		return guidance;
	}

}