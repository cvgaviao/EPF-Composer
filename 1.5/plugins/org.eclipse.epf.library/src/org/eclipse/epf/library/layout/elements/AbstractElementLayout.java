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
package org.eclipse.epf.library.layout.elements;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.epf.common.utils.FileUtil;
import org.eclipse.epf.common.utils.NetUtil;
import org.eclipse.epf.common.utils.StrUtil;
import org.eclipse.epf.library.ILibraryResourceManager;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.configuration.ElementRealizer;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.layout.ElementLayoutManager;
import org.eclipse.epf.library.layout.IElementLayout;
import org.eclipse.epf.library.layout.LayoutInfo;
import org.eclipse.epf.library.layout.LayoutResources;
import org.eclipse.epf.library.layout.util.XmlElement;
import org.eclipse.epf.library.persistence.ILibraryResourceSet;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.library.util.ResourceHelper;
import org.eclipse.epf.persistence.MultiFileXMISaveImpl;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.Concept;
import org.eclipse.epf.uma.ContentDescription;
import org.eclipse.epf.uma.ContentElement;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.DescribableElement;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.Example;
import org.eclipse.epf.uma.Guideline;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ReusableAsset;
import org.eclipse.epf.uma.Roadmap;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.epf.uma.Whitepaper;
import org.eclipse.epf.uma.WorkOrder;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.epf.uma.ecore.util.OppositeFeature;
import org.eclipse.epf.uma.util.AssociationHelper;
import org.eclipse.epf.uma.util.UmaUtil;

/**
 * The abstract layout for all Method Elements.
 * 
 * @author Jinhua Xi
 * @author Kelvin Low
 * @sicne 1.0
 */
public abstract class AbstractElementLayout implements IElementLayout {

	public static final String TAG_REFERENCE = "reference"; //$NON-NLS-1$
	public static final String TAG_REFERENCELIST = "referenceList"; //$NON-NLS-1$
	
	protected ElementLayoutManager layoutManager;

	protected MethodElement element;

	protected MethodElement ownerElement;

	protected String url = null;

	// element element path relative to the publishing root
	protected String elementPath = ""; //$NON-NLS-1$

	// the back up path to reath the publishing root
	protected String backPath = ""; //$NON-NLS-1$

	// map of layout file to the output file extention, if the element has more
	// than one layout output
	protected List layouts = null;

	protected MethodElement targetElement = null;

	protected boolean showElementLink = true;
	
	protected boolean debug = LibraryPlugin.getDefault().isDebugging();

	public AbstractElementLayout() {
	}

	/**
	 * if the element's content is target for another element, set it here. for
	 * example, step content cat target for a Task or a task descriptor
	 * copyright content can target to different elements.
	 * 
	 * The purpose of this is that the system will fix the links in the content
	 * to relative to the target element.
	 */
	public void setContentTarget(MethodElement targetElement) {
		this.targetElement = targetElement;
	}

	/**
	 * need to set the owner of the current layout element. In most cases this
	 * should be the eContainer of the element. This is needed because in some
	 * situation the element does not have an owner when the object is created.
	 * For example, the ContentDescription object's eContiner is null if the
	 * content file is not saved.
	 * 
	 * @param owner
	 *            MethodElement
	 */
	public void setElementOwner(MethodElement owner) {
		this.ownerElement = owner;
	}

	/**
	 * @deprecated
	 */
	public void setShowElementLink(boolean show) {
		this.showElementLink = show;
	}
	
	/**
	 * initialize the layout with the element layout manager and the element.
	 * @param layoutManager ElementLayoutManager
	 * @param element MethodElement
	 */
	public abstract void init(ElementLayoutManager layoutManager,
			MethodElement element);

	protected void __init(ElementLayoutManager layoutManager,
			MethodElement element) {
		this.layoutManager = layoutManager;
		this.element = element;
		if (!ConfigurationHelper.isDescriptionElement(element)) {
			if ( !(element instanceof MethodConfiguration) ) {
				// make sure the element is loaded with the correct container
				// otherwise the  element may not have a valid container
				// and thus the element content path can't be determined
				// this is just a workaround. should we have a better way to make sure the elements are loaded correctly???????????
				// 160441 - Missing template files when publish configuration
				MethodPlugin plugin = LibraryUtil.getMethodPlugin(element);
				if ( plugin == null || plugin.eContainer() == null) {
					
					// this should not happen in publishing mode since the whole library is already loaded
					if ( debug && getLayoutMgr().isPublishingMode() ) {
						System.out.println("Error: Element without a valid container: " + LibraryUtil.getTypeName(element)); //$NON-NLS-1$
					}
					
					LibraryUtil.getMethodPlugins(LibraryService.getInstance().getCurrentMethodLibrary());
				} 
			}
			buildPath();
			this.url = elementPath + getFileName(ResourceHelper.FILE_EXT_HTML);
		}
		
		// this is a workaround. 
		// we need to pass parameters in the file url, 
		// however, if the file does not exist, the parameters is not passed over in browsing
		// so create a dummy file
		if (!this.layoutManager.isPublishingMode() && !(
					element instanceof ContentDescription 
				|| element.eContainer() instanceof ContentDescription
				|| element instanceof MethodConfiguration) )
		{
			try {
				String html_file = this.layoutManager.getPublishDir() + this.getFilePath() + getFileName(ResourceHelper.FILE_EXT_HTML);
				File f = new File(html_file);
				if ( !f.exists() )
				{
					f.getParentFile().mkdirs();
					f.createNewFile();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		preloadOppositefeatures();

	}

	private void preloadOppositefeatures() {
		// make sure the processes are loaded before calculating the feature
		// otherwise the opposite feature map is not populated
		// fine for publishing since the whole library is loaded already
		// special handling for browsing
		// load the opposite feature specifically
		if ( getLayoutMgr().isPublishingMode() ) {
			return;
		}
		
		//OppositeFeature of;
		Resource res = element.eResource();
		if ( res == null ) {
			return;
		}
		
		ResourceSet resSet = res.getResourceSet();
		if ( !(resSet instanceof ILibraryResourceSet) ) {
			return;
		}
		
		List<OppositeFeature> oppositeFeatures = new ArrayList<OppositeFeature>();
		if ( this.element instanceof Role ) {
			oppositeFeatures.add(AssociationHelper.Role_RoleDescriptors);
		} else if ( this.element instanceof Task ) {
			oppositeFeatures.add(AssociationHelper.Task_TaskDescriptors);
		} else if ( this.element instanceof WorkProduct ) {
			oppositeFeatures.add(AssociationHelper.WorkProduct_WorkProductDescriptors);
		} else if ( this.element instanceof Checklist ) {
			oppositeFeatures.add(AssociationHelper.Checklist_BreakdownElements);
		} else if ( this.element instanceof Concept || this.element instanceof Whitepaper ) {
			oppositeFeatures.add(AssociationHelper.Concept_BreakdownElements);
		} else if ( this.element instanceof Checklist ) {
			oppositeFeatures.add(AssociationHelper.Checklist_BreakdownElements);
		} else if ( this.element instanceof Example ) {
			oppositeFeatures.add(AssociationHelper.Example_BreakdownElements);
		} else if ( this.element instanceof Guideline ) {
			oppositeFeatures.add(AssociationHelper.Guideline_BreakdownElements);
		} else if ( this.element instanceof ReusableAsset ) {
			oppositeFeatures.add(AssociationHelper.ReusableAsset_BreakdownElements);
		} else if ( this.element instanceof Roadmap ) {
			oppositeFeatures.add(AssociationHelper.Roadmap_Activites);
		} else if ( this.element instanceof SupportingMaterial ) {
			oppositeFeatures.add(AssociationHelper.SupportingMaterial_BreakdownElements);
		} 
	
		if ( oppositeFeatures.size() > 0 ) {
			Set<String> GUIDs = new HashSet<String>();
			GUIDs.add(element.getGuid());
			((ILibraryResourceSet)resSet).loadOppositeFeatures(oppositeFeatures, GUIDs);
		}
	}

	protected void setLayoutInfo(LayoutInfo info) {
		if (layouts == null) {
			layouts = new ArrayList();
		}
		layouts.add(info);
	}

	/**
	 * return a list of LayoutInfo objects
	 * 
	 * @return a list of LayoutInfo objects
	 */
	public List getLayouts() {
		return layouts;
	}

	/**
	 * get layout by name
	 * 
	 * @param name
	 * @return LayoutInfo
	 */
	public LayoutInfo getLayoutInfo(String name) {
		if ( layouts == null || layouts.size() == 0 ) {
			return null;
		}
		
		for (Iterator it = layouts.iterator(); it.hasNext(); ) {
			LayoutInfo info = (LayoutInfo)it.next();
			if ( info.name.equals(name) ) {
				return info;
			}
		}
		
		return null;
	}
	
	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getLayoutMgr()
	 */
	public ElementLayoutManager getLayoutMgr() {
		return this.layoutManager;
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getElement()
	 */
	public MethodElement getElement() {
		return element;
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getType()
	 */
	public String getType() {
		return element.getType().getName();
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getName()
	 */
	public String getName() {
		return element.getName();
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getDisplayName()
	 */
	public String getDisplayName() {
		
		// [Bug 196399] P-name is not inherited from its base element in an extending element
		return ConfigurationHelper.getPresentationName(element, getLayoutMgr().getConfiguration());
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getId()
	 */
	public String getId() {
		return element.getGuid();
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getUrl()
	 */
	public String getUrl() {
		return url;
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getBackPath()
	 */
	public String getBackPath() {
		return backPath;
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getFilePath()
	 */
	public String getFilePath() {
		return elementPath;
	}

	/**
	 * Returns the file path relative to another element. This is the
	 * relativeTo.backpath + this.elementpath.
	 *
	 * @see org.eclipse.epf.library.layout.IElementLayout#getFilePath(IElementLayout)
	 */

	public String getFilePath(IElementLayout relativeTo) {
		return relativeTo.getBackPath() + this.getFilePath();
	}

	/**
	 * Returns the file name with the given extension.
	 * 
	 * @see org.eclipse.epf.library.layout.IElementLayout#getFileName(String)
	 */

	public String getFileName(String ext) {
		return ResourceHelper.getFileName(element, ext);
	}



	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getXslUrl()
	 */
	public String getXslUrl() {
		if (element instanceof ContentElement) {
			return LayoutResources.getXslUri(getType().toLowerCase(),
					"contentelement"); //$NON-NLS-1$
		} else {
			return LayoutResources.getXslUri(getType().toLowerCase(), null);
		}
	}

	private void buildPath() {
		// this is <plugin>/<element type>/
		elementPath = ""; //$NON-NLS-1$
		backPath = "./"; //$NON-NLS-1$
		if ( !(element instanceof MethodConfiguration || element instanceof MethodLibrary) )
		{
			String path = ResourceHelper.getElementPath(element);
			if ( path != null ) {
				elementPath = path.replace(File.separatorChar, '/');
				backPath = ResourceHelper.getBackPath(element);
			}

		}
	}

	/**
	 * check if the html content generated from this xsl file needs to be scanned or not
	 * scan the content is for identifying element references in the content and copy over resource files
	 * in some cases we don't need to scan the content, for example, the activity breakdown structure
	 * 
	 * @param xslUrl the xsl that html is generated from, null for the default xsl layout
	 * @return boolean
	 */
	public boolean needContentScan(String xslUrl)
	{
		// by default all contents are scanned
		return true;
	}
	
	/**
	 * get the layout for a child element of this element
	 * ActivityElementLayout should override this method to create layout with node path
	 * @param child
	 * @return IElementLayout
	 */
	protected IElementLayout getChildLayout(MethodElement child)
	{
		return layoutManager.getLayout(child, true);
	}
	
	protected void processChild(Object feature, XmlElement parent, MethodElement e,
			boolean includeReferences) {
		if (e != null) {
			IElementLayout l = getChildLayout(e);
			if (l != null) {
				// don't include the references of the refereced elements,
				// otherwise, may cause deadlock
				boolean isContent = (e instanceof MethodElement)
						&& ConfigurationHelper
								.isDescriptionElement((MethodElement) e);
				if (isContent) {
					if (targetElement != null) {
						l.setContentTarget(targetElement);
					}
					l.setElementOwner(element);
				}
				parent.addChild(l.getXmlElement(includeReferences));
			}
		}
	}

	protected void processChild(Object feature, XmlElement parent, List items,
			boolean includeReferences) {
		if (items != null && items.size() > 0) {
			for (Iterator it = items.iterator(); it.hasNext();) {
				Object e = it.next();
				if (e instanceof MethodElement) {
					MethodElement me = (MethodElement) e;
					e = ConfigurationHelper.getCalculatedElement(me,
							layoutManager.getConfiguration());
					if (e != null) {
						IElementLayout l = getChildLayout(me);
						if (l != null) {							
							// don't include the references of the refereced
							// elements, otherwise, may cause deadlock
							parent.addChild(l.getXmlElement(ConfigurationHelper
									.isDescriptionElement(me) ? true
									: includeReferences));
						}
					}
				}
			}
		}
	}

	/**
	 * calculate the to-many references
	 * @param elementXml XmlElement
	 * @param includeReferences boolean
	 */
	public void calculate0nReferences(XmlElement elementXml,
			boolean includeReferences) {
		// referenceMap.clear();
		List features = LibraryUtil.getStructuralFeatures(element);
//		List properties = element.getInstanceProperties();
		if (features != null) {
			// get all string type attributes
			for (int i = 0; i < features.size(); i++) {
				EStructuralFeature feature = (EStructuralFeature) features
						.get(i);
				if (feature.isMany()) {
					loadFeature(feature, elementXml, includeReferences);
				}
			}
		}
	}

	protected XmlElement getXmlElement() {
		XmlElement elementXml = new XmlElement("Element") //$NON-NLS-1$
				.setAttribute("Id", getId()) //$NON-NLS-1$	
				.setAttribute("Type", getType()) //$NON-NLS-1$
				.setAttribute("TypeName", TngUtil.getTypeText(element)) //$NON-NLS-1$
				.setAttribute("Name", getName()) //$NON-NLS-1$
				.setAttribute("BackPath", getBackPath()) //$NON-NLS-1$
				.setAttribute("ShapeiconUrl", getShapeiconUrl()) //$NON-NLS-1$
				.setAttribute("DisplayName", getDisplayName()); //$NON-NLS-1$

		if ( showElementLink ) {
			elementXml.setAttribute("Url", getUrl()); //$NON-NLS-1$
		} 
		
		return elementXml;
	}

	/**
	 * load the attributes
	 * @param elementXml XmlElement
	 */
	public void loadAttributes(XmlElement elementXml) {

		boolean isActivityAttribute = (element instanceof Activity) 
			|| (element instanceof ContentDescription) 
				&& (element.eContainer() instanceof Activity || ownerElement instanceof Activity);
		
		List features = LibraryUtil.getStructuralFeatures(element);
//		List properties = element.getInstanceProperties();
		if (features != null) {
			// get all string type attributes
			for (int i = 0; i < features.size(); i++) {
				EStructuralFeature p = (EStructuralFeature) features.get(i);			
				if (!(p instanceof EAttribute) ) {
					continue;
				}
				EAttribute attrib = (EAttribute)p;
				String name = p.getName();
									
				Object value;
				if (name.equals("presentationName")) //$NON-NLS-1$
				{
					// value = TngUtil.getPresentationName(element);
					value = ConfigurationHelper.getPresentationName(element,
							layoutManager.getConfiguration());
				} else if( isActivityAttribute && String.class.isAssignableFrom(attrib.getEAttributeType().getInstanceClass())) {
					
					// for activity, the way to merge attribute from base is different
					value = ConfigurationHelper.getActivityStringAttribute(element, ownerElement, attrib, getLayoutMgr().getConfiguration());
				} else {
					value = getAttributeFeatureValue(p);
				}
				
				// escape Brief Description into HTML entities
				if (name.equals("briefDescription") && value instanceof String) { //$NON-NLS-1$
					MultiFileXMISaveImpl.MyEscape myEscape = new MultiFileXMISaveImpl.MyEscape();
					value = myEscape.convert((String)value);
					value = StrUtil.convertNewlinesToHTML((String)value);
				}

				elementXml
						.newChild("attribute").setAttribute("name", name).setValue((value == null) ? "" : value.toString()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		}
	}

	/**
	 * get the attribute feature value and merge the text, fix url referneces, etc.
	 * @param feature EStructuralFeature
	 * @return Object
	 */
	public Object getAttributeFeatureValue(EStructuralFeature feature) {
		Object value = ConfigurationHelper.calcAttributeFeatureValue(element,
				ownerElement, feature, layoutManager.getConfiguration());

		// now if the target is set and is different from the element or the
		// element's owner
		// then fix the content
		if ((targetElement != null) && (value != null)
				&& (value.toString().length() > 0)) {
			String contentPath = ResourceHelper
					.getElementPath((element instanceof ContentDescription) ? 
							((ownerElement==null) ? (MethodElement) element.eContainer() : ownerElement) 
							: element);

			String backPath = ResourceHelper
					.getBackPath((targetElement instanceof ContentDescription) ? (MethodElement) targetElement
							.eContainer()
							: targetElement);

			value = ResourceHelper.fixContentUrlPath(value.toString(),
					contentPath, backPath);

		}

		return value;
	}

	/**
	 * load the copyright text
	 * @param elementXml XmlElement
	 */
	public void loadCopyright(XmlElement elementXml) {

		// List items = new ArrayList();
		// ConfigurationHelper.calculateCopyright(element,
		// layoutManager.getConfiguration(), items);
		// if ( items.size() > 0 )
		// {
		// SupportingMaterial copyright;
		// StringBuffer copyrights = new StringBuffer();
		// for ( Iterator it = items.iterator(); it.hasNext(); )
		// {
		// copyright = (SupportingMaterial) it.next();
		// String statement = copyright.getPresentation().getMainDescription();
		//							
		// // need to fix the content for relative links.
		// // since the link is a relative path to the SupportingMaterial
		// location,
		// // need to convert to relative to the current element
		// // so re-calcuate the back path
		// // jxi, 06/28/05
		// String contentPath = ResourceHelper.getElementPath(copyright);
		// String backPath = ResourceHelper.getBackPath(element);
		// statement = ResourceHelper.fixContentUrlPath(statement, contentPath,
		// backPath);
		// copyrights.append(statement);
		// }
		//			
		// elementXml.newChild("copyright").setValue(copyrights.toString());
		// //$NON-NLS-1$
		// }

		String copyright = ConfigurationHelper.getCopyrightText(element,
				layoutManager.getConfiguration());
		if (copyright != null && copyright.length() > 0) {
			elementXml.newChild("copyright").setValue(copyright); //$NON-NLS-1$
		}
	}

	/**
	 * calculate the to-one reference
	 * @param elementXml XmlElement
	 * @param includeReferences boolean
	 */
	public void calculate01References(XmlElement elementXml,
			boolean includeReferences) {
		List features = LibraryUtil.getStructuralFeatures(element);
//		List properties = element.getInstanceProperties();
		if (features != null) {
			// get element references
			for (int i = 0; i < features.size(); i++) {
				EStructuralFeature p = (EStructuralFeature) features.get(i);

				// Object value = element.get(i);

				EClassifier type = p.getEType();
				if (!(type instanceof EClass) || p.isMany()) {
					continue;
				}

				loadFeature(p, elementXml, includeReferences);
			}
		}
	}

	/**
	 * load the non-attribute feature
	 * 
	 * @param feature
	 * @param elementXml
	 * @param includeReferences
	 */
	public void loadFeature(EStructuralFeature feature, XmlElement elementXml,
			boolean includeReferences) {
		getFeatureValue(feature,  elementXml, includeReferences);
	}
	
	/**
	 * load the non-attribute feature
	 * 
	 * @param feature
	 * @param elementXml
	 * @param includeReferences
	 */
	public Object getFeatureValue(EStructuralFeature feature, XmlElement elementXml,
			boolean includeReferences) {
		
		if (!(feature.getEType() instanceof EClass)) {
			return null;
		}

		String name = feature.getName();

		if (!feature.isMany()) {
			MethodElement e = ConfigurationHelper.calc01FeatureValue(element, ownerElement,
					feature, layoutManager.getElementRealizer());
			// Browsing stops working when a role is set to
			// replaced another role
			// for replacer, the base will be evaluated to the replacer
			// and causing deadlock
			if (e != null && e != element) {
				boolean showDetail = (ConfigurationHelper
						.isDescriptionElement(e)
				/*
				 * || (p ==
				 * UmaPackage.eINSTANCE.getMethodUnit_CopyrightStatement())
				 */) ? true : includeReferences;
				
				if ( acceptFeatureValue(feature, e) ) {
					if ( elementXml != null ) {
						processChild(feature, elementXml
										.newChild(TAG_REFERENCE).setAttribute("name", name), e, showDetail); //$NON-NLS-1$ 
					}
					
					return e;
				} else {
					return null;
				}
			}
		} else if (feature.isMany()) {
			List pv = ConfigurationHelper.calc0nFeatureValue(element, ownerElement, feature,
					layoutManager.getElementRealizer());
			if ( acceptFeatureValue(feature, pv) ) {
				if ( elementXml != null ) {
					addReferences(feature, elementXml, name, pv);
				}
				return pv;
			} else {
				return null;
			}
		}
		
		return null;
	}

	/**
	 * load the non-attribute feature
	 * 
	 * @param feature
	 * @param elementXml
	 * @param includeReferences
	 */
	public void loadFeature(OppositeFeature feature, XmlElement elementXml,
			boolean includeReferences) {
		String name = feature.getName();

		if (!feature.isMany()) {
			MethodElement e = ConfigurationHelper.calc01FeatureValue(element,
					feature, layoutManager.getElementRealizer());
			// Browsing stops working when a role is set to
			// replaced another role
			// for replacer, the base will be evaluated to the replacer
			// and causing deadlock
			if (e != null && e != element) {
				boolean showDetail = (ConfigurationHelper
						.isDescriptionElement(e)
				/*
				 * || (p ==
				 * UmaPackage.eINSTANCE.getMethodUnit_CopyrightStatement())
				 */) ? true : includeReferences;

				if ( acceptFeatureValue(feature, e) )
				{
					processChild(feature, 
							elementXml
									.newChild(TAG_REFERENCE).setAttribute("name", name), e, showDetail); //$NON-NLS-1$ 
				}
			}
		} else if (feature.isMany()) {
			List pv = calc0nFeatureValue(element, feature,
					layoutManager.getElementRealizer());
			if ( acceptFeatureValue(feature, pv) && pv.size() > 0) {
				addReferences(feature, elementXml, name, pv);
			}
		}
	}
	
	protected List calc0nFeatureValue(MethodElement element,
			OppositeFeature feature, ElementRealizer realizer) {
		return ConfigurationHelper.calc0nFeatureValue(element, feature,
				layoutManager.getElementRealizer());
	}
	
	/**
	 * load references for the element
	 * @param elementXml XmlElement
	 * @param includeReferences boolean
	 */
	public void loadReferences(XmlElement elementXml, boolean includeReferences) {
		List features = LibraryUtil.getStructuralFeatures(element);
//		List properties = element.getInstanceProperties();
		if (features != null) {
			for (int i = 0; i < features.size(); i++) {
				EStructuralFeature feature = (EStructuralFeature) features
						.get(i);
				if (feature.getEType() instanceof EClass) {
					loadFeature(feature, elementXml, includeReferences);
				}
			}
		}
		
		Collection oppositeProperties = new ArrayList(element.getOppositeFeatures());
		for (Iterator z= oppositeProperties.iterator(); z.hasNext(); )
		{
			OppositeFeature ofeature = (OppositeFeature) z.next();
			loadFeature(ofeature, elementXml, includeReferences);
		}
		//elementXml.setAttribute("ShowFullMethodContent", (layoutManager.getValidator().showExtraInfoForDescriptors()) ? "true" : "false");
	}

	/**
	 * add the reference layout to the result
	 * @param feature Object
	 * @param elementXml XmlElement
	 * @param referenceName String
	 * @param element MethodElement
	 */
	public void addReference(Object feature, XmlElement elementXml, String referenceName,
			MethodElement element) {
		processChild(feature, 
				elementXml
						.newChild(TAG_REFERENCE).setAttribute("name", referenceName), element, false); //$NON-NLS-1$ 
	}

	/**
	 * add references to the layout
	 * @param feature
	 * @param elementXml
	 * @param referenceName
	 * @param items
	 */
	public void addReferences(Object feature, XmlElement elementXml, String referenceName,
			List items) {
		processChild(feature, 
				elementXml
						.newChild(TAG_REFERENCELIST).setAttribute("name", referenceName), items, false); //$NON-NLS-1$ 
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getXmlElement(boolean)
	 */
	public XmlElement getXmlElement(boolean includeReferences) {
		XmlElement elementXml = getXmlElement();

		// load the references
		if (includeReferences) {

			// load the copyright info
			loadCopyright(elementXml);

			// load the attributes
			loadAttributes(elementXml);

			loadReferences(elementXml, false);
		}

		return elementXml;
	}

	/**
	 * some layout need to have the feature values for further processing. So
	 * this method will be called when a feature is calculated in this abstract
	 * class
	 * 
	 * @param name
	 * @param value
	 */
	protected boolean acceptFeatureValue(EStructuralFeature feature, Object value) {
		return true;
	}

	protected boolean acceptFeatureValue(OppositeFeature feature, Object value) {
		if ( feature == AssociationHelper.DescribableElement_CustomCategories) {
			if ( value instanceof CustomCategory ) {
				return getPublishCategoryProperty((MethodElement)value);
			} else if ( value instanceof List) {
				List items = (List)value;
				int i = 0;
				while ( i < items.size() ) {
					MethodElement e = (MethodElement)items.get(i);
					if ( !getPublishCategoryProperty(e) ) {
						items.remove(i);
					} else {
						i++;
					}
				}
				
				return true;
			}
		}
			
		return true;
	}
	
	private boolean getPublishCategoryProperty(MethodElement e) {
		MethodElementProperty prop = TngUtil.getPublishCategoryProperty(e);
		if ( prop == null ) {
			return false;
		}
		
		String v = prop.getValue();
		if ( v == null || !v.toString().equals("true") ) { //$NON-NLS-1$
			return false;
		}
	
		return true;
	}
	
	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getShapeiconUrl()
	 */
	public String getShapeiconUrl() {
		URI uri = null;
		String imageUrl;
		
		// String imageFile;
		if (element instanceof DescribableElement) {
			uri = ((DescribableElement) element).getShapeicon();
		}

		ILibraryResourceManager resMgr = ResourceHelper.getResourceMgr(element);

		if (uri == null || resMgr == null ) {
			imageUrl = getDefaultShapeiconUrl();
		} else {
			imageUrl = NetUtil.decodedFileUrl(uri.toString());
		}

		// need to copy the image file if it's not in the default directory
		if (!imageUrl.startsWith("images/")) //$NON-NLS-1$
		{
		
			// From EPF 1.2, shapeIcon/nodeIcon stores uri relative to plugin path. 
			// Below section of code is necessary to append plugin name to shapeIcon/nodeIcon uri.
			MethodPlugin plugin = UmaUtil.getMethodPlugin(element);
			if(plugin != null){
				if(!imageUrl.startsWith(plugin.getName()) ){
//					String pluginPath = ResourceHelper.getPluginPath(element);
//					File file = new File(pluginPath + File.separator+ imageUrl);
//					imageUrl = NetUtil.encodeFileURL(FileUtil.getRelativePath(file, 
//							new File(libRoot)));
					imageUrl = NetUtil.encodeFileURL(plugin.getName() + "/" + imageUrl); //$NON-NLS-1$
				}
			}
			
			if ( resMgr != null ) {
				// need decoded URL for file ops
				String decodedImageUrl = NetUtil.decodedFileUrl(imageUrl);
				File source = new File(resMgr.resolve(element, decodedImageUrl));
				File dest = new File(getLayoutMgr().getPublishDir(), decodedImageUrl);
				FileUtil.copyFile(source, dest);
			}
		}

		return imageUrl;
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getNodeiconUrl()
	 */
	public String getNodeiconUrl() {
		return ""; //$NON-NLS-1$
	}

	public String getDefaultShapeiconUrl() {
		return LayoutResources.getDefaultShapeiconUrl(element.getType()
				.getName().toLowerCase());
	}

	/**
	 * @see org.eclipse.epf.library.layout.IElementLayout#getDiagramiconUrl()
	 */
	public String getDiagramiconUrl() {
		return "icons/" + element.getType().getName() + ".gif"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * this is a shared method to load the work order of the work breakdown
	 * element. will be used by all work breakdown elemnt layout.
	 * 
	 * @param elementXml
	 *            XmlElement the parent xml element to load the work order
	 *            layout
	 */
	protected void loadWorkOrder(XmlElement elementXml) {
		EStructuralFeature feature = UmaPackage.eINSTANCE
				.getWorkBreakdownElement_LinkToPredecessor();

		List items = ConfigurationHelper.calc0nFeatureValue(element, feature,
				layoutManager.getElementRealizer());
		XmlElement predecessorXml = elementXml
				.newChild(TAG_REFERENCELIST).setAttribute("name", feature.getName()); //$NON-NLS-1$ 
		if (items != null && items.size() > 0) {
			for (Iterator it = items.iterator(); it.hasNext();) {
				WorkOrder wo = (WorkOrder) it.next();
				MethodElement me = (MethodElement) ConfigurationHelper
						.calc01FeatureValue(wo, UmaPackage.eINSTANCE
								.getWorkOrder_Pred(), layoutManager
								.getElementRealizer());
				if (me != null) {
					IElementLayout l = getChildLayout(me);
					if (l != null) {
						predecessorXml.addChild(l.getXmlElement(false));
					}
				}
			}
		}
	}
	
	/**
	 * show the descriptors of the method element
	 * @param descriptors
	 */
	protected void processDescriptors(XmlElement elementXml) {
		
		// 180583 - Pub options to allow adding (when publishing) all descriptors under a method element in form of their path
		
		if ( !getLayoutMgr().getValidator().showRelatedDescriptors() ) {
			return;
		}
		
		// show the role descriptors referencing this role
		OppositeFeature of = null;
		
		if ( this.element instanceof Role ) {
			of = AssociationHelper.Role_RoleDescriptors;
		} else if ( this.element instanceof Task ) {
			of = AssociationHelper.Task_TaskDescriptors;
		} else if ( this.element instanceof WorkProduct ) {
			of = AssociationHelper.WorkProduct_WorkProductDescriptors;
		}
			
		List descriptors = ConfigurationHelper.calc0nFeatureValue(
				this.element, of,
				layoutManager.getElementRealizer());

		// create a element nodes for cescriptor list
		XmlElement descListXml = elementXml.newChild(TAG_REFERENCELIST).setAttribute("name", "descriptors"); //$NON-NLS-1$ //$NON-NLS-2$
		
		for (int i = 0; i < descriptors.size(); i++ ) {
			Descriptor desc = (Descriptor)descriptors.get(i);
			IElementLayout layout = getChildLayout(desc);
			XmlElement descXml = layout.getXmlElement(false);
			descListXml.addChild(descXml);
			
			List parents = __getSuperActivities(desc);
			for (int p = 0; p < parents.size(); p++ ) {
				Activity act = (Activity)parents.get(p);
				layout = getChildLayout(act);
				XmlElement actXml = layout.getXmlElement(false);
				descXml.addChild(actXml);
			}			
		}
	}
	
	private List __getSuperActivities(BreakdownElement element) {
		List items = new ArrayList();
		
		// build the parent nodes
		BreakdownElement parent = element.getSuperActivities(); 
		while ( parent != null ) {
			
			// need to realize the activity, so that we get the correct link and parent links
			// 204745 - wrong process usage path for contributing activities
			parent = (BreakdownElement)ConfigurationHelper.getCalculatedElement(parent, layoutManager.getElementRealizer());
			if ( parent == null ) {
				break;
			}

			items.add(0, parent);
			if (LibraryUtil.isProcess(parent) ) {
				break;
			}
			parent = parent.getSuperActivities();
		}

		return items;
	}
	
	protected boolean isBreakdownElement_Guidances(EStructuralFeature feature) {
		UmaPackage umaPkg = UmaPackage.eINSTANCE;
		if (feature == umaPkg.getBreakdownElement_Checklists()) {
			return true;
		}
		if (feature == umaPkg.getBreakdownElement_Concepts()) {
			return true;
		}
		if (feature == umaPkg.getBreakdownElement_Examples()) {
			return true;
		}
		if (feature == umaPkg.getBreakdownElement_Guidelines()) {
			return true;
		}	
		if (feature == umaPkg.getBreakdownElement_ReusableAssets()) {
			return true;
		}	
		if (feature == umaPkg.getBreakdownElement_SupportingMaterials()) {
			return true;
		}		
		
		return false;
	}
	
}