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
package org.eclipse.epf.library.tester.impl.testcommands;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.edit.command.AbstractOverrideableCommand;
import org.eclipse.epf.library.LibraryServiceUtil;
import org.eclipse.epf.library.tester.TesterOutputUtil;
import org.eclipse.epf.library.tester.iface.TCExeReply;
import org.eclipse.epf.library.tester.impl.TestCommandImpl;
import org.eclipse.epf.services.ILibraryPersister;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.BreakdownElement;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.ContentElement;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.DeliveryProcess;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPackage;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.ProcessElement;
import org.eclipse.epf.uma.ProcessPackage;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;
import org.eclipse.swt.widgets.Display;
import org.w3c.dom.Element;

/**
 * A test command class
 * 
 * @author Weiping Lu
 * @since 1.0
 * 
 */
public class TCNewMethodElement extends TestCommandImpl {				
	
	private static Map typeClassMap = new HashMap();	
	static {
		UmaFactory uf = UmaFactory.eINSTANCE;
		typeClassMap.put("MethodPlugin", uf.createMethodPlugin().eClass());
		typeClassMap.put("MethodConfiguration", uf.createMethodConfiguration().eClass());
		typeClassMap.put("ContentPackage", uf.createContentPackage().eClass());
		typeClassMap.put("Role", uf.createRole().eClass());
		typeClassMap.put("Task", uf.createTask().eClass());
		typeClassMap.put("Artifact", uf.createArtifact().eClass());
		typeClassMap.put("Deliverable", uf.createDeliverable().eClass());
		typeClassMap.put("Outcome", uf.createOutcome().eClass());
		typeClassMap.put("Concept", uf.createConcept().eClass());
		typeClassMap.put("Checklist", uf.createChecklist().eClass());
		typeClassMap.put("Example", uf.createExample().eClass());
		typeClassMap.put("Guideline", uf.createGuideline().eClass());
		typeClassMap.put("Report", uf.createReport().eClass());
		typeClassMap.put("Template", uf.createTemplate().eClass());
		typeClassMap.put("SupportingMaterial", uf.createSupportingMaterial().eClass());
		typeClassMap.put("ToolMentor", uf.createToolMentor().eClass());
		typeClassMap.put("Whitepaper", uf.createWhitepaper().eClass());
		typeClassMap.put("Practice", uf.createPractice().eClass());
		typeClassMap.put("ReusableAsset", uf.createReusableAsset().eClass());
		typeClassMap.put("Roadmap", uf.createRoadmap().eClass());
		typeClassMap.put("TermDefinition", uf.createTermDefinition().eClass());
				
		typeClassMap.put("Discipline", uf.createDiscipline().eClass());
		typeClassMap.put("DisciplineGrouping", uf.createDisciplineGrouping().eClass());
		typeClassMap.put("Domain", uf.createDomain().eClass());		
		typeClassMap.put("WorkProductType", uf.createWorkProductType().eClass());		
		typeClassMap.put("RoleSet", uf.createRoleSet().eClass());
		typeClassMap.put("RoleSetGrouping", uf.createRoleSetGrouping().eClass());
		typeClassMap.put("Tool", uf.createTool().eClass());
		typeClassMap.put("CustomCategory", uf.createCustomCategory().eClass());
		
		typeClassMap.put("ProcessPackage", uf.createProcessPackage().eClass());
		typeClassMap.put("Activity", uf.createActivity().eClass());
		typeClassMap.put("Phase", uf.createPhase().eClass());
		typeClassMap.put("Iteration", uf.createIteration().eClass());		
		typeClassMap.put("CapabilityPattern", uf.createCapabilityPattern().eClass());
		typeClassMap.put("DeliveryProcess", uf.createDeliveryProcess().eClass());
		typeClassMap.put("TaskDescriptor", uf.createTaskDescriptor().eClass());
		
	}
	
	private TestCommandImpl delegate;
		
	public void parse(Element element) {
		super.parse(element);
		if (element.getAttribute("type").equals("MethodPlugin")) {
			delegate = new TCNewMethodPlugin();
			delegate.setOwner(getOwner());
			delegate.parse(element);
			return;
		}
		
		super.parse(element);
		setAttribute(AT_Type, element.getAttribute(AT_Type));
		setAttribute(AT_Name, element.getAttribute(AT_Name));
		setAttribute(AT_ParentPath, element.getAttribute(AT_ParentPath));
	}	
	
	public TCExeReply execute() {
		if (delegate != null) {
			return delegate.execute();
		}

		String type = getAttribute(AT_Type);
		String name = getAttribute(AT_Name);
		String parentPath = getAttribute(AT_ParentPath);
		
		EClass eClass = getEClass(type);
		MethodElement me = (MethodElement) UmaFactory.eINSTANCE.create(eClass);
		me.setName(name);
		me.setGuid(EcoreUtil.generateUUID());
		TCExeReply reply = addToLib(parentPath, me);
		if (reply != null && !reply.passing()) {
			return reply;
		}
		return executeEdit(name, parentPath);
	}

	private TCExeReply executeEdit(String name, String parentPath) {		
		//To do: check to see if edit is needed. Return if not needed.
		
		TestCommandImpl edit = new TCEditMethodElement();
		edit.setOwner(getOwner());
		edit.parse(getElement());
		String path = parentPath == null || parentPath.length() == 0 ? name  : parentPath + "/" + name;		
		edit.removeAttribute(AT_Name);
		edit.removeAttribute(AT_ParentPath);
		edit.setAttribute(AT_Path, path);
		
		return edit.execute();
	}

	private TCExeReply addToLib(String parentPath, MethodElement me) {
		if (me instanceof BreakdownElement) {
			return addProcessElementToLib(parentPath, (ProcessElement) me);
		}
		MethodLibrary currLib = getOwner().getCurrentBaseLib();
		MethodElement parentMe = TesterOutputUtil.getMethodElement(currLib, parentPath);
		if (parentMe == null) {
			return new TCExeReplyImpl("parentMe == null", false);
		}		
		addToParent(me, parentMe);

		save(me);
		//save(parentMe);
		
		return null;
	}
	
	private TCExeReply addProcessElementToLib(String parentPath, ProcessElement pe) {
		MethodLibrary currLib = getOwner().getCurrentBaseLib();
		ProcessPackage parentMe = TesterOutputUtil.getProcessElementParent(currLib, parentPath);
		if (parentMe == null) {
			return new TCExeReplyImpl("parentMe == null", false);
		}
		if (pe instanceof CapabilityPattern || pe instanceof DeliveryProcess) {			

			ProcessComponent pc = UmaFactory.eINSTANCE.createProcessComponent();
			pc.setGuid(EcoreUtil.generateUUID());
			pc.setName(pe.getName());
			parentMe.getChildPackages().add(pc);
			pc.setProcess((org.eclipse.epf.uma.Process) pe);			
			addToParent(pe, pc);
			
		} else if (pe instanceof BreakdownElement) {
			BreakdownElement be = (BreakdownElement) pe;
			ProcessPackage pkg = UmaFactory.eINSTANCE.createProcessPackage();
			pkg.setName(pe.getName());
			parentMe.getChildPackages().add(pkg);;
			addToParent(pe, pkg);
			
			Activity parentAct = parentMe instanceof ProcessComponent ? 
					((ProcessComponent) parentMe).getProcess() :
					(Activity) parentMe.getProcessElements().get(0);
			parentAct.getBreakdownElements().add(be);
		}

		save(parentMe);
		
		return null;
	}
		

	private void addToParent(MethodElement me, MethodElement parentMe) {
		EStructuralFeature feature = getContainmentFeature(parentMe, me);
		if (feature == null) {
			System.out.println("\nWarning: containment feature not registered!");
			System.out.println(  "Warning: parentMe: " + parentMe);
			System.out.println(  "Warning:       me: " + me);
		} else if (feature.isMany()) {
			EList elist = AbstractOverrideableCommand.getOwnerList(parentMe, feature); 
			List c = new ArrayList();
			c.add(me);		
			elist.addAll(c);
		} else {
			parentMe.eSet(feature, me);
		}
	}
	
	public EClass getEClass(String type) {
		EClass ret = (EClass) typeClassMap.get(type);
		if (ret == null) {
			System.out.println("\nWarning: \"" + type + 
					"\" is not registered in \"typeClassMap\"!");
		}
		return ret;		
	}
	
	private EStructuralFeature getContainmentFeature(EObject owner, EObject eobj) {
		UmaPackage up = UmaPackage.eINSTANCE;
		
		if (owner instanceof MethodLibrary) {
			if (eobj instanceof MethodPlugin) {
				return up.getMethodLibrary_MethodPlugins();
			}
			if (eobj instanceof MethodConfiguration) {
				return up.getMethodLibrary_PredefinedConfigurations();
			}
		}		
		
		if (owner instanceof ContentPackage) {
			if (eobj instanceof ContentElement) {
				return up.getContentPackage_ContentElements();
			}
		}
		
		if (owner instanceof ProcessComponent) {
			if (eobj instanceof org.eclipse.epf.uma.Process) {
				return up.getProcessComponent_Process();
			}
		}
				
		if (owner instanceof ProcessPackage) {
			if (eobj instanceof ProcessElement) {
				return up.getProcessPackage_ProcessElements();
			}
		}
		
		if (owner instanceof MethodPackage) {
			if (eobj instanceof ContentPackage ||
				eobj instanceof ProcessPackage	) {
				return up.getMethodPackage_ChildPackages();
			}
		}
		
		return null;
	}
	
	public void save(MethodElement element) {
		final ILibraryPersister.FailSafeMethodLibraryPersister persister = LibraryServiceUtil
				.getPersisterFor(element.eResource()).getFailSafePersister();
		final Resource res = element.eResource();
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				try {
					persister.save(res);
					persister.commit();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}
	
}