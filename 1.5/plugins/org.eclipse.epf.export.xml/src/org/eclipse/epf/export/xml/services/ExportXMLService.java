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
package org.eclipse.epf.export.xml.services;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.epf.dataexchange.util.ContentProcessor;
import org.eclipse.epf.dataexchange.util.IResourceHandler;
import org.eclipse.epf.diagram.ui.service.DiagramImageService;
import org.eclipse.epf.export.services.DiagramHandler;
import org.eclipse.epf.export.xml.ExportXMLPlugin;
import org.eclipse.epf.export.xml.ExportXMLResources;
import org.eclipse.epf.library.IConfigurationManager;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.edit.IFilter;
import org.eclipse.epf.library.edit.util.MethodElementPropertyHelper;
import org.eclipse.epf.library.edit.util.ModelStructure;
import org.eclipse.epf.library.edit.util.Suppression;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.library.layout.ElementLayoutManager;
import org.eclipse.epf.library.services.SafeUpdateController;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.library.util.MigrationUtil;
import org.eclipse.epf.library.util.ResourceHelper;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Diagram;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodElementProperty;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.WorkBreakdownElement;
import org.eclipse.epf.uma.WorkOrder;
import org.eclipse.epf.uma.WorkOrderType;
import org.eclipse.epf.uma.ecore.IModelObject;
import org.eclipse.epf.uma.util.UmaUtil;
import org.eclipse.osgi.util.NLS;

/**
 * XmlExport service class to process the exporting task
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class ExportXMLService {

	// String xmlPath;
	private ExportXMLData data;
	
	private boolean debug = ExportXMLPlugin.getDefault().isDebugging();

	private XMLLibrary xmlLib;

	private List discardedElements = new ArrayList();

	ExportXMLLogger logger = null;

	// in case we need to mapping the uma objects to xml objects
	// such as the content categories
	private Map umaIdToXmlIdMap = new HashMap();
	
	private DiagramHandler diagramHandler;
	
	//tds with steps
	private Map<String, TaskDescriptor> tdMap;
	
	private Set<WorkOrder> successors;

	/**
	 * Creates a new instance.
	 */
	public ExportXMLService(ExportXMLData data) {
		this.data = data;
		logger = new ExportXMLLogger(new File(this.data.xmlFile)
				.getParentFile());
	}
	/**
	 * Gets the logger file.
	 */
	public File getLogPath() {
		return logger.getLogPath();
	}

	/**
	 * Dispose
	 */
	public void dispose() {
		logger.dispose();
	}

	/**
	 * Does XML library export.
	 */
	public void doExport(IProgressMonitor monitor) {
		try {

			IResourceHandler handler = new ExportResourceHandler(
					new File(LibraryService.getInstance()
							.getCurrentMethodLibraryLocation()), new File(data
							.getXMLFile()).getParentFile());
			ContentProcessor contentProc = new ContentProcessor(handler,
					this.logger);
			
			diagramHandler = new DiagramHandler(
					new File(LibraryService.getInstance()
							.getCurrentMethodLibraryLocation()), new File(data
							.getXMLFile()).getParentFile()) {
				
				public void visitElementMap(int passId) {
					if (passId != 0) {
						return;
					}
/*					String libFolderPath = getSourceLibRoot().getAbsolutePath();
					int ix = libFolderPath.length();*/
					
					Map<String, MapEntryValue> map = getElementMap();
					for (Iterator<MapEntryValue> it = map.values().iterator(); it.hasNext(); ) {
						MapEntryValue value = it.next();
						if (value.targetFile == null) {
							continue;
						} else if (value.sourceFile != null) {
							IModelObject xmlObj = getXmlObject(value.element);
							if (xmlObj instanceof org.eclipse.epf.xml.uma.Process) {
								org.eclipse.epf.xml.uma.Process proc = 
													(org.eclipse.epf.xml.uma.Process) xmlObj;
								String uri = value.sourceFile.getAbsolutePath();
								String srcRootFolderPath = getRoot(value.element).getAbsolutePath();
								int ix = srcRootFolderPath.length();
								
								uri = uri.substring(ix + 1);
								uri = URI.createFileURI(uri).toString();
								proc.setDiagramURI(uri);
							}		
						} 
					}	
				}
				
				public void execute() {
					super.execute();
					SafeUpdateController.syncExec(new Runnable() {
						public void run() {
							exportDiagramImageFiles();
						}
					});
				}

				private void exportDiagramImageFiles() {
					String folderPath = LibraryService.getInstance().getCurrentMethodLibraryLocation();
					
					DiagramImageService diagramService = new DiagramImageService(null);
					diagramService.setForXMLExport(true);
					
					try {
						Map<String, MapEntryValue> map = getElementMap();
						for (Iterator<MapEntryValue> it = map.values().iterator(); it.hasNext(); ) {
							MapEntryValue value = it.next();
							org.eclipse.epf.uma.Process proc = (org.eclipse.epf.uma.Process) value.element;
							MethodConfiguration config = proc.getDefaultContext();
							if (config == null) {
								continue;
							}
							diagramService.setConfig(config);
							IConfigurationManager mgr = LibraryService.getInstance().getConfigurationManager(config);
							if (mgr == null) {
								continue;
							}
							ElementLayoutManager layoutMgr = mgr.getElementLayoutManager();
							if (layoutMgr == null) {
								continue;
							}
							IFilter filter = layoutMgr.getDiagramAdapterFactory().getFilter();
							
							File diagramXmiFile = value.targetFile;
							if (diagramXmiFile == null || !diagramXmiFile.exists()) {
								continue;
							}
							diagramService.setPubDir(diagramXmiFile.getParentFile());
							
							Map activities = MigrationUtil.getActivities(proc, true);	
							for (Activity act: (Collection<Activity>) activities.values()) {
								String[] types = {	ResourceHelper.DIAGRAM_TYPE_WORKFLOW,
													ResourceHelper.DIAGRAM_TYPE_ACTIVITY_DETAIL,
													ResourceHelper.DIAGRAM_TYPE_WP_DEPENDENCY};
								
								for (int i = 0; i < types.length; i++) {
									String imagePath = ResourceHelper.getXmlExportedDiagramImageFileName(act, types[i], ResourceHelper.FILE_EXT_JPEG);
									try {
										diagramService.saveDiagram(act, imagePath, types[i], filter, Suppression.getSuppression(proc));
									} catch (Exception e) {
										if (debug) {
											logger.logError(NLS.bind(ExportXMLResources.exportXMLService_error,
													e.getMessage()), e);
										}
									}
								}
							}
						}
						
					} catch (Exception e) {
						if (debug) {
							logger.logError(NLS.bind(ExportXMLResources.exportXMLService_error,
									e.getMessage()), e);
						}
					} finally {										
						diagramService.dispose();
					}
				}
			};

			// FeatureManager.INSTANCE.printFeatureNameMapping();

			// export the current library to the xml file
			// get the current library instance and export all to the xml
			MethodLibrary src = LibraryService.getInstance()
					.getCurrentMethodLibrary();

			this.xmlLib = new XMLLibrary(contentProc, logger, data
					.getXMLFile());

			this.xmlLib.createLibrary(src.getGuid(), src.getName());
			IModelObject target = this.xmlLib.getRoot();

			creatEDataObjectTree(src, target);
			iteratEDataObject(src);
			handleSuccessors();

			this.xmlLib.fixLibraryForExport();
			this.xmlLib.fixTaskDescriptorsForExport(tdMap);
			
			diagramHandler.visitElementMap(0);
			if (data.exportType == ExportXMLData.EXPORT_METHOD_PLUGINS) {
				xmlLib.storeExtraInfo(src.getMethodPlugins());
			}
			this.xmlLib.save();
			diagramHandler.execute();

		} catch (Exception e) {
			logger.logError(NLS.bind(ExportXMLResources.exportXMLService_error,
					e.getMessage()), e);
		}
	}

	private boolean isPluginSelected(MethodPlugin plugin) {
		if (data.exportType == ExportXMLData.EXPORT_METHOD_LIBRARY) {
			return true;
		} else if (data.exportType == ExportXMLData.EXPORT_METHOD_PLUGINS) {
			return (data.selectedPlugins != null)
					&& data.selectedPlugins.contains(plugin);
		} else if (data.exportType == ExportXMLData.EXPORT_METHOD_CONFIGS) {
			//return false;
			return true;	//hot fix
		} else {
			return true;
		}
	}

	private boolean isConfigExcluded(MethodConfiguration config) {
		if (data.exportType == ExportXMLData.EXPORT_METHOD_PLUGINS) {
			return data.associatedConfigs == null || 
					!data.associatedConfigs.contains(config.getGuid());
		} 
		return false;
	}
	
	private void creatEDataObjectTree(MethodElement srcObj,
			IModelObject targetObj) {

		// if it's a plugin, skip all the system packages
		// and find the top most user packages
		if (srcObj instanceof MethodPlugin) {

			MethodPlugin plugin = (MethodPlugin) srcObj;
			if (!isPluginSelected(plugin)) {
				return;
			}

			// all system packages are discarded
			discardedElements.addAll(TngUtil.getAllSystemPackages(plugin));

			List topmostUserPackages = getTopmostUserPackage(plugin);
			List categoryPkgs = TngUtil.getContentCategoryPackages(plugin);

			org.eclipse.epf.xml.uma.ContentCategoryPackage xmlPkg = (org.eclipse.epf.xml.uma.ContentCategoryPackage) this.xmlLib
					.getContentCategoryPackage(plugin.getGuid());
			String contentCategoryPackageId = xmlPkg.getId();

			for (Iterator itp = topmostUserPackages.iterator(); itp.hasNext();) {
				MethodPackage pkg = (MethodPackage) itp.next();

				// if it's a category package,
				// create all contained elements in the single custom category
				// package
				if (categoryPkgs.contains(pkg)) {

					// set the it mapping
					setIdMapping(pkg.getGuid(), contentCategoryPackageId);
					creatEDataObjectTree(pkg, xmlPkg);
				} else {
					creatEDataObjectTree(pkg, targetObj);
				}
			}
		} else {
			for (Iterator it = srcObj.eContents().iterator(); it.hasNext();) {
				Object nextObj = it.next();
				MethodElement child = nextObj instanceof MethodElement ?
						(MethodElement) nextObj : null;
				if (child == null) {
					continue;
				}

				// in uma model, WorkOrder is contained in package
				// in xml model, WorkOrder is contained by the owning
				// WorkBreakdownElement,
				// so delay the creation if the owner is a package
				if (child instanceof WorkOrder
						&& srcObj instanceof MethodPackage) {
					WorkOrder workOrder = (WorkOrder) child;
					MethodElementProperty prop = MethodElementPropertyHelper
					.getProperty(
							workOrder,
							MethodElementPropertyHelper.WORK_ORDER__SUCCESSOR);
					if (prop != null) {
						getSuccessors().add(workOrder);
					}					
					continue;
				}

				if ((child instanceof MethodPlugin)
						&& !isPluginSelected((MethodPlugin) child)) {
					continue;
				}
				
				if (child instanceof MethodConfiguration &&
					isConfigExcluded((MethodConfiguration) child)) {
					continue;
				}

				if (!isDiscarded(child)) {
					createXmlObject(child, targetObj);
				}
			}
		}
	}

	private void setIdMapping(String umaId, String xmlId) {
		if (!umaIdToXmlIdMap.containsKey(umaId)) {
			umaIdToXmlIdMap.put(umaId, xmlId);
		}
	}

	private String getXmlId(String umaId) {
		if (umaIdToXmlIdMap.containsKey(umaId)) {
			return (String) umaIdToXmlIdMap.get(umaId);
		}
		return umaId;
	}

	private boolean isDiscarded(Object o) {
		if (o instanceof Diagram) {
			return true;
		}
		return discardedElements.contains(o);
	}

	private IModelObject getXmlObject(MethodElement srcObj) {
		String id = srcObj.getGuid();
		if (umaIdToXmlIdMap.containsKey(id)) {
			id = (String) umaIdToXmlIdMap.get(id);
		}
		return this.xmlLib.getElement(id);
	}

	/**
	 * create an xml element for the container, based on the uma element
	 * 
	 * @param umaElement
	 * @param targetContainer
	 */
	private void createXmlObject(MethodElement umaElement,
			IModelObject targetContainer) {
		EStructuralFeature feature = umaElement.eContainmentFeature();
		createXmlObject(umaElement, targetContainer, feature.getName());
	}

	/**
	 * create an xml element for the container, based on the uma element
	 * 
	 * @param umaElement
	 * @param targetContainer
	 */
	private void createXmlObject(MethodElement umaElement,
			IModelObject targetContainer, String containmentFeature) {

		try {
			if (umaElement == null) {
				return;
			}

			// get the containment feature so we can create the object of the
			// same type
			String elementType = umaElement.getType().getName();
			IModelObject xmlElement = xmlLib.createElement(targetContainer,
					containmentFeature, umaElement.eClass().getName(),
					elementType, umaElement.getGuid());
			
			if (umaElement instanceof TaskDescriptor) {					
				TaskDescriptor td = (TaskDescriptor) umaElement;
				if (td.getTask() != null) {
					List steps = td.getSelectedSteps();
					if (steps != null && !steps.isEmpty()) {
						if (tdMap == null) {
							tdMap = new HashMap<String, TaskDescriptor>();
						}
						tdMap.put(td.getGuid(), td);
					}
				}			
			}
			
			if (xmlElement != null) {
				if (umaElement instanceof WorkOrder
						&& xmlElement instanceof org.eclipse.epf.xml.uma.WorkOrder) {
					xmlLib.getSuccessOrWorkOrderMap().put(
							(org.eclipse.epf.xml.uma.WorkOrder) xmlElement,
							(WorkOrder) umaElement);
				}
				
				// recursive
				creatEDataObjectTree(umaElement, xmlElement);
			}

		} catch (Exception e) {
			// log the error and continue???
			e.printStackTrace();
		}
	}

	private void iteratEDataObject(MethodElement srcObj) {
		diagramHandler.registerElement(srcObj, true);
		
		if (srcObj instanceof MethodPlugin) {
			MethodPlugin plugin = (MethodPlugin) srcObj;
			if (!isPluginSelected(plugin)) {
				return;
			}
		}

		if (isProcessed(srcObj.getGuid())) {
			return;
		}
		setProcessed(srcObj.getGuid());

		IModelObject targetObj = getXmlObject(srcObj);

		// if object is not created,
		// which means it's either a system package or something wrong
		if (targetObj == null) {
			if (srcObj instanceof MethodPackage) {
				// iterate the child packages
				for (Iterator it = ((MethodPackage) srcObj).getChildPackages()
						.iterator(); it.hasNext();) {
					iteratEDataObject((MethodElement) it.next());
				}
			}

			// // elements can be reference in other plugins
			// // don't need to care about this
			// else if (!isDiscarded(srcObj) ) {
			// logger.logWarning("Warning: xml object not created for " +
			// LibraryUtil.getTypeName(srcObj) );
			// }

			return;
		}

		EClass eClass = srcObj.eClass();

		EList features = eClass.getEAllStructuralFeatures();
		for (Iterator it = features.iterator(); it.hasNext();) {
			EStructuralFeature feature = (EStructuralFeature) it.next();

			Object value = srcObj.eGet(feature);

			try {
				if (value instanceof List && ! feature.getName().equals("methodElementProperty")) {//$NON-NLS-1$	
					for (Iterator itv = ((List) value).iterator(); itv
							.hasNext();) {
						MethodElement src_value = (MethodElement) itv.next();

						// handle the
						// UmaPackage#getWorkBreakdownElement_LinkToPredecessor
						// feature value
						// the WorkOrder in uma is a process element in a
						// package
						// in xml model, it's an EDataObject contained by the
						// WorkBreakdownElement
						if (src_value instanceof WorkOrder) {
							// get the owner of the WorkOrder
							IModelObject workOrder = getXmlObject(src_value);
							if (workOrder == null) {
								createXmlObject(src_value, targetObj, feature
										.getName());
							}
							// System.out.println("workorder value");
						}

						// set the reference
						String xmlId = getXmlId(src_value.getGuid());
						xmlLib.setReferenceValue(targetObj, feature.getName(),
								xmlId, src_value.getType());

						iteratEDataObject(src_value);
					}
				} else if (value instanceof MethodElement) {
					// get the xml object and iterate again
					MethodElement src_value = (MethodElement) value;

					// if ( src_value instanceof WorkOrder ) {
					// System.out.println("workorder value");
					// }

					// elements like Presentation may not have a Content
					// Description
					// the one returned may not have a container
					if (src_value.eContainer() != null) {
						String xmlId = getXmlId(src_value.getGuid());
						xmlLib.setReferenceValue(targetObj, feature.getName(),
								xmlId, src_value.getType());

						iteratEDataObject(src_value);
					}
				} else if (!isDiscarded(srcObj)) {
					// if ( targetObj instanceof ContentCategoryPackage) {
					// System.out.println("hrre");
					// }

					if (value instanceof WorkOrderType) {
						value = ((WorkOrderType) value).getName();
					}

					xmlLib.setAtributeFeatureValue(targetObj,
							feature.getName(), value);
				}
			} catch (Exception e) {
				String msg = NLS.bind(
						ExportXMLResources.exportXMLService_feature_error,
						LibraryUtil.getTypeName(srcObj), feature.getName());
				logger.logError(msg, e);
			}

		}

	}

	// goid of processed element
	private List processedElements = new ArrayList();

	private boolean isProcessed(String guid) {
		return processedElements.contains(guid);
	}

	private void setProcessed(String guid) {
		if (!processedElements.contains(guid)) {
			processedElements.add(guid);
		}
	}

	private List getTopmostUserPackage(MethodPlugin plugin) {

		MethodPackage pkg_core_content = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.coreContentPath);

		MethodPackage pkg_custom_categories = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.customCategoryPath);

		MethodPackage pkg_disciplines = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.disciplineDefinitionPath);

		MethodPackage pkg_domains = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.domainPath);

		MethodPackage pkg_rolesets = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.roleSetPath);

		MethodPackage pkg_tools = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.toolPath);

		MethodPackage pkg_wptypes = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.workProductTypePath);

		MethodPackage pkg_cp = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.capabilityPatternPath);

		MethodPackage pkg_dp = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.deliveryProcessPath);

		MethodPackage pkg_proc_contribution = UmaUtil.findMethodPackage(plugin,
				ModelStructure.DEFAULT.processContributionPath);

		List pkgs = new ArrayList();
		pkgs.add(pkg_core_content);
		pkgs.add(pkg_custom_categories);
		pkgs.add(pkg_disciplines);
		pkgs.add(pkg_domains);
		pkgs.add(pkg_rolesets);
		pkgs.add(pkg_tools);
		pkgs.add(pkg_wptypes);
		pkgs.add(pkg_cp);
		pkgs.add(pkg_dp);

		if ( pkg_proc_contribution != null ) {
			pkgs.add(pkg_proc_contribution);
		}
		
		return pkgs;

		// List systemPkgs = TngUtil.getAllSystemPackages(plugin);
		//		
		// List unprocessedPkgs = new ArrayList(plugin.getMethodPackages());
		// while (unprocessedPkgs.size() > 0 ) {
		// MethodPackage pkg = (MethodPackage) unprocessedPkgs.remove(0);
		// if ( !systemPkgs.contains(pkg) ) {
		// if ( !pkgs.contains(pkg) ) {
		// pkgs.add(pkg);
		// }
		// continue;
		// }
		//			
		// for ( Iterator it = pkg.getChildPackages().iterator(); it.hasNext();
		// ) {
		// MethodPackage p = (MethodPackage)it.next();
		// if ( systemPkgs.contains(p) )
		// {
		// if ( !unprocessedPkgs.contains(p) ) {
		// unprocessedPkgs.add(p);
		// }
		// } else {
		// if ( !pkgs.contains(p) ) {
		// pkgs.add(p);
		// }
		// }
		// }
		//			
		// if ( unprocessedPkgs.size() == 0 ) {
		// break;
		// }
		// }
		//		
		// // need to include the content packages
		// pkgs.addAll(TngUtil.getContentCategoryPackages(plugin));

	}
	public ExportXMLLogger getLogger() {
		return logger;
	}
	
	private Set<WorkOrder> getSuccessors() {
		if (successors == null) {
			successors = new HashSet<WorkOrder>();
		}
		return successors;
	}
	
	private void  handleSuccessors() {
		if (getSuccessors() == null) {
			return;
		}
		
		String fname = "linkToPredecessor";	//$NON-NLS-1$
		for (WorkOrder srcWorkOrder : getSuccessors()) {
			WorkBreakdownElement wbe = srcWorkOrder.getPred();
			if (wbe == null) {
				continue;
			}			
			IModelObject targetObj = getXmlObject(wbe);
			if (targetObj == null) {
				continue;
			}			
			IModelObject tgtWorkOrder = getXmlObject(srcWorkOrder);
			if (tgtWorkOrder == null) {
				createXmlObject(srcWorkOrder, targetObj, fname);
			}			
			String xmlId = getXmlId(srcWorkOrder.getGuid());
			if (xmlId == null) {
				continue;
			}
			try {
				xmlLib.setReferenceValue(targetObj, fname, xmlId, srcWorkOrder
						.getType());
			} catch (Exception e) {
				String msg = NLS.bind(
						ExportXMLResources.exportXMLService_feature_error,
						LibraryUtil.getTypeName(wbe), fname);
				logger.logError(msg, e);
			}
		}
	}

	// private boolean isSystemPackage(EDataObject element) {
	//		
	// if ( !(element instanceof MethodPackage) ) {
	// return false;
	// }
	//		
	// MethodPlugin plugin = LibraryUtil.getMethodPlugin(element);
	// if ( plugin == null ) {
	// return false;
	// }
	//		
	// return TngUtil.getAllSystemPackages(plugin).contains(element);
	//		
	// }
}
