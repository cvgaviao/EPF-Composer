//------------------------------------------------------------------------------
// Copyright (c) 2005, 2007 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.search.ui.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.epf.library.ui.LibraryUIText;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.impl.ActivityImpl;
import org.eclipse.epf.uma.impl.ArtifactImpl;
import org.eclipse.epf.uma.impl.CapabilityPatternImpl;
import org.eclipse.epf.uma.impl.ChecklistImpl;
import org.eclipse.epf.uma.impl.ConceptImpl;
import org.eclipse.epf.uma.impl.ContentCategoryImpl;
import org.eclipse.epf.uma.impl.ContentPackageImpl;
import org.eclipse.epf.uma.impl.CustomCategoryImpl;
import org.eclipse.epf.uma.impl.DeliverableImpl;
import org.eclipse.epf.uma.impl.DeliveryProcessImpl;
import org.eclipse.epf.uma.impl.DisciplineGroupingImpl;
import org.eclipse.epf.uma.impl.DisciplineImpl;
import org.eclipse.epf.uma.impl.DomainImpl;
import org.eclipse.epf.uma.impl.EstimationConsiderationsImpl;
import org.eclipse.epf.uma.impl.ExampleImpl;
import org.eclipse.epf.uma.impl.GuidanceImpl;
import org.eclipse.epf.uma.impl.GuidelineImpl;
import org.eclipse.epf.uma.impl.IterationImpl;
import org.eclipse.epf.uma.impl.MethodPluginImpl;
import org.eclipse.epf.uma.impl.MilestoneImpl;
import org.eclipse.epf.uma.impl.OutcomeImpl;
import org.eclipse.epf.uma.impl.PhaseImpl;
import org.eclipse.epf.uma.impl.PracticeImpl;
import org.eclipse.epf.uma.impl.ProcessComponentImpl;
import org.eclipse.epf.uma.impl.ProcessPackageImpl;
import org.eclipse.epf.uma.impl.ReportImpl;
import org.eclipse.epf.uma.impl.ReusableAssetImpl;
import org.eclipse.epf.uma.impl.RoadmapImpl;
import org.eclipse.epf.uma.impl.RoleDescriptorImpl;
import org.eclipse.epf.uma.impl.RoleImpl;
import org.eclipse.epf.uma.impl.RoleSetGroupingImpl;
import org.eclipse.epf.uma.impl.RoleSetImpl;
import org.eclipse.epf.uma.impl.SupportingMaterialImpl;
import org.eclipse.epf.uma.impl.TaskDescriptorImpl;
import org.eclipse.epf.uma.impl.TaskImpl;
import org.eclipse.epf.uma.impl.TemplateImpl;
import org.eclipse.epf.uma.impl.TermDefinitionImpl;
import org.eclipse.epf.uma.impl.ToolImpl;
import org.eclipse.epf.uma.impl.ToolMentorImpl;
import org.eclipse.epf.uma.impl.WhitepaperImpl;
import org.eclipse.epf.uma.impl.WorkProductDescriptorImpl;
import org.eclipse.epf.uma.impl.WorkProductTypeImpl;

/**
 * The method search scope.
 * 
 * @author Kelvin Low
 * @since 1.0
 */
public class MethodSearchScope {

	public static final String ROOT = "Root"; //$NON-NLS-1$

	public static final String METHOD_CONTENT = LibraryUIText.TEXT_METHOD_CONTENT;

	public static final String ROLE = LibraryUIText.TEXT_ROLE;

	public static final String TASK = LibraryUIText.TEXT_TASK;

	public static final String WORK_PRODUCT = LibraryUIText.TEXT_WORK_PRODUCT;

	public static final String GUIDANCE = LibraryUIText.TEXT_GUIDANCE;

	public static final String CHECKLIST = LibraryUIText.TEXT_CHECKLIST;

	public static final String CONCEPT = LibraryUIText.TEXT_CONCEPT;

	public static final String ESTIMATION_CONSIDERATIONS = LibraryUIText.TEXT_ESTIMATION_CONSIDERATIONS;

	public static final String EXAMPLE = LibraryUIText.TEXT_EXAMPLE;

	public static final String GUIDELINE = LibraryUIText.TEXT_GUIDELINE;

	public static final String PRACTICE = LibraryUIText.TEXT_PRACTICE;

	public static final String REPORT = LibraryUIText.TEXT_REPORT;

	public static final String REUSABLE_ASSET = LibraryUIText.TEXT_REUSABLE_ASSET;

	public static final String ROADMAP = LibraryUIText.TEXT_ROADMAP;

	public static final String SUPPORTING_MATERIAL = LibraryUIText.TEXT_SUPPORTING_MATERIAL;

	public static final String TEMPLATE = LibraryUIText.TEXT_TEMPLATE;

	public static final String TERM_DEFINITION = LibraryUIText.TEXT_TERM_DEFINITION;

	public static final String TOOL_MENTOR = LibraryUIText.TEXT_TOOL_MENTOR;

	public static final String WHITEPAPER = LibraryUIText.TEXT_WHITEPAPER;

	public static final String STANDARD_CATEGORY = LibraryUIText.TEXT_STANDARD_CATEGORY;

	public static final String CUSTOM_CATEGORY = LibraryUIText.TEXT_CUSTOM_CATEGORY;

	public static final String PROCESS = LibraryUIText.TEXT_PROCESS;

	public static final String CAPABILITY_PATTERN = LibraryUIText.TEXT_CAPABILITY_PATTERN;

	public static final String DELIVERY_PROCESS = LibraryUIText.TEXT_DELIVERY_PROCESS;

	private static Map<Class, String> elementSearchScope = new HashMap<Class, String>();

	static {
		elementSearchScope.put(MethodPluginImpl.class, ROOT);
		elementSearchScope.put(ContentPackageImpl.class, METHOD_CONTENT);
		elementSearchScope.put(RoleImpl.class, ROLE);
		elementSearchScope.put(TaskImpl.class, TASK);
		elementSearchScope.put(ArtifactImpl.class, WORK_PRODUCT);
		elementSearchScope.put(DeliverableImpl.class, WORK_PRODUCT);
		elementSearchScope.put(OutcomeImpl.class, WORK_PRODUCT);
		elementSearchScope.put(GuidanceImpl.class, GUIDANCE);
		elementSearchScope.put(ChecklistImpl.class, CHECKLIST);
		elementSearchScope.put(ConceptImpl.class, CONCEPT);
		elementSearchScope.put(EstimationConsiderationsImpl.class,
				ESTIMATION_CONSIDERATIONS);
		elementSearchScope.put(ExampleImpl.class, EXAMPLE);
		elementSearchScope.put(GuidelineImpl.class, GUIDELINE);
		elementSearchScope.put(PracticeImpl.class, PRACTICE);
		elementSearchScope.put(ReportImpl.class, REPORT);
		elementSearchScope.put(ReusableAssetImpl.class, REUSABLE_ASSET);
		elementSearchScope.put(RoadmapImpl.class, ROADMAP);
		elementSearchScope.put(SupportingMaterialImpl.class,
				SUPPORTING_MATERIAL);
		elementSearchScope.put(TemplateImpl.class, TEMPLATE);
		elementSearchScope.put(TermDefinitionImpl.class, TERM_DEFINITION);
		elementSearchScope.put(ToolMentorImpl.class, TOOL_MENTOR);
		elementSearchScope.put(WhitepaperImpl.class, WHITEPAPER);
		elementSearchScope.put(ContentCategoryImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(DisciplineImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(DisciplineGroupingImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(DomainImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(WorkProductTypeImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(RoleSetImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(RoleSetGroupingImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(ToolImpl.class, STANDARD_CATEGORY);
		elementSearchScope.put(CustomCategoryImpl.class, CUSTOM_CATEGORY);
		elementSearchScope.put(ProcessPackageImpl.class, PROCESS);
		elementSearchScope.put(ProcessComponentImpl.class, PROCESS);
		elementSearchScope.put(PhaseImpl.class, PROCESS);
		elementSearchScope.put(IterationImpl.class, PROCESS);
		elementSearchScope.put(MilestoneImpl.class, PROCESS);
		elementSearchScope.put(ActivityImpl.class, PROCESS);
		elementSearchScope.put(RoleDescriptorImpl.class, PROCESS);
		elementSearchScope.put(TaskDescriptorImpl.class, PROCESS);
		elementSearchScope.put(WorkProductDescriptorImpl.class, PROCESS);
		elementSearchScope.put(CapabilityPatternImpl.class, CAPABILITY_PATTERN);
		elementSearchScope.put(DeliveryProcessImpl.class, DELIVERY_PROCESS);
	}

	private List<Object> searchScope = new ArrayList<Object>();

	/**
	 * Creates a new instance.
	 */
	public MethodSearchScope(Object[] elementTypes) {
		searchScope.add(ROOT);
		for (int i = 0; i < elementTypes.length; i++) {
			Object elementType = elementTypes[i];
			searchScope.add(elementType);
		}
	}

	/**
	 * Checks whether a core method element is included in the search scope.
	 * 
	 * @return <code>true</code> if a core method element is included in the
	 *         search scope, <code>false</code> otherwise
	 */
	public boolean includeCoreContent() {
		return searchScope.contains(ROLE) || searchScope.contains(TASK)
				|| searchScope.contains(WORK_PRODUCT)
				|| searchScope.contains(GUIDANCE)
				|| searchScope.contains(CHECKLIST)
				|| searchScope.contains(CONCEPT)
				|| searchScope.contains(ESTIMATION_CONSIDERATIONS)
				|| searchScope.contains(EXAMPLE)
				|| searchScope.contains(GUIDELINE)
				|| searchScope.contains(PRACTICE)
				|| searchScope.contains(REPORT)
				|| searchScope.contains(REUSABLE_ASSET)
				|| searchScope.contains(ROADMAP)
				|| searchScope.contains(SUPPORTING_MATERIAL)
				|| searchScope.contains(TEMPLATE)
				|| searchScope.contains(TERM_DEFINITION)
				|| searchScope.contains(TOOL_MENTOR)
				|| searchScope.contains(WHITEPAPER);
	}

	/**
	 * Checks whether the given Method element is included in the search scope.
	 * 
	 * @param element
	 *            a method element
	 * @return <code>true</code> if the method element is included in the
	 *         search scope, <code>false</code> otherwise
	 */
	public boolean include(MethodElement element) {
		if (element == null)
			return false;
		String searchScopeName = (String) elementSearchScope.get(element
				.getClass());
		return searchScope.contains(searchScopeName);
	}

	/**
	 * Checks whether the given method element type is included in the search
	 * scope.
	 * 
	 * @return <code>true</code> if the given method element type is included
	 *         in the search scope, <code>false</code> otherwise
	 */
	public boolean include(String searchScopeName) {
		return searchScope.contains(searchScopeName);
	}

}
