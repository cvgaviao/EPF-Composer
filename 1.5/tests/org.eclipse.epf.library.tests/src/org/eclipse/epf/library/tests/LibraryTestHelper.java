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
package org.eclipse.epf.library.tests;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.epf.common.utils.FileUtil;
import org.eclipse.epf.dataexchange.importing.LibraryService;
import org.eclipse.epf.dataexchange.importing.PluginService;
import org.eclipse.epf.library.LibraryServiceException;
import org.eclipse.epf.library.util.LibraryUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Artifact;
import org.eclipse.epf.uma.CapabilityPattern;
import org.eclipse.epf.uma.Checklist;
import org.eclipse.epf.uma.ContentDescription;
import org.eclipse.epf.uma.ContentPackage;
import org.eclipse.epf.uma.CustomCategory;
import org.eclipse.epf.uma.Discipline;
import org.eclipse.epf.uma.Domain;
import org.eclipse.epf.uma.Guidance;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;
import org.eclipse.epf.uma.ProcessComponent;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.RoleSet;
import org.eclipse.epf.uma.Section;
import org.eclipse.epf.uma.SupportingMaterial;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.VariabilityElement;
import org.eclipse.epf.uma.VariabilityType;
import org.eclipse.epf.uma.WorkProductType;

import com.ibm.icu.util.Calendar;

/**
 * Helper class to manipulate a test method library.
 * 
 * @author Jinhua Xi
 * @since 1.0
 */
public class LibraryTestHelper {

	public static final String TEMP_TEST_DIR = System.getProperty("user.home") + File.separator //$NON-NLS-2$
			+ "EPF" + File.separator + "test" + File.separator; //$NON-NLS-1$ //$NON-NLS-2$
	
	private static final String LIBRARY_XMI = "library.xmi"; //$NON-NLS-1$
	
	private static final String XMI_TYPE = "xmi"; //$NON-NLS-1$		

	/**
	 * Creates a new test method library.
	 */
	public static MethodLibrary createTestMethodLibrary()
			throws LibraryServiceException {
		String path = TEMP_TEST_DIR
				+ Long.toHexString(Calendar.getInstance().getTimeInMillis())
				+ File.separator;
		
		org.eclipse.epf.library.LibraryService.getInstance()
				.closeCurrentMethodLibrary();
		Map params = new HashMap();
		params.put("library.path", path); //$NON-NLS-1$
		return org.eclipse.epf.library.LibraryService.getInstance()
				.createMethodLibrary(LIBRARY_XMI, XMI_TYPE, params);
	}

	/**
	 * Creates a test method library on a given path.
	 */
	public static MethodLibrary createTestMethodLibrary(String path)
			throws LibraryServiceException {
		if (path == null) {
			path = TEMP_TEST_DIR
					+ Long
							.toHexString(Calendar.getInstance()
									.getTimeInMillis()) + File.separator;
		}
		
		// If the library path already exists, delete it.
		File libraryPath = new File(path);
		if (libraryPath.exists()) {
			FileUtil.deleteAllFiles(libraryPath.getAbsolutePath());
			libraryPath.delete();			
		}
		
		org.eclipse.epf.library.LibraryService.getInstance()
				.closeCurrentMethodLibrary();
		Map params = new HashMap();
		params.put("library.path", path); //$NON-NLS-1$
		return org.eclipse.epf.library.LibraryService.getInstance()
				.createMethodLibrary(LIBRARY_XMI, XMI_TYPE, params);
	}

	/**
	 * Closes the current method library and delete the library resource files.
	 */
	public static void closeLibrary() {
		try {
			File path = new File(org.eclipse.epf.library.LibraryService
					.getInstance().getCurrentMethodLibraryLocation());
			org.eclipse.epf.library.LibraryService.getInstance()
					.closeCurrentMethodLibrary();

			// Delete the library folder and files.
			FileUtil.deleteAllFiles(path.getAbsolutePath());
			path.delete();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public static void saveLibrary() {
		try {
			org.eclipse.epf.library.LibraryService.getInstance()
					.saveCurrentMethodLibrary();
		} catch (LibraryServiceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static MethodConfiguration createConfiguration(String name) {
		return LibraryService.INSTANCE.createConfiguration(name, EcoreUtil
				.generateUUID());
	}

	public static MethodPlugin createMethodPlugin(String name) {
		try {
			return LibraryService.INSTANCE.createPlugin(name, EcoreUtil
					.generateUUID());
		} catch (Exception ex) {
			ex.printStackTrace();
		}

		return null;
	}

	public static Discipline createDiscipline(MethodPlugin plugin, String name) {
		Discipline element = LibraryService.INSTANCE
				.createPluginService(plugin).createDiscipline();
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static RoleSet createRoleSet(MethodPlugin plugin, String name) {
		RoleSet element = LibraryService.INSTANCE.createPluginService(plugin)
				.createRoleSet();
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Domain createDomain(MethodPlugin plugin, String name) {
		Domain element = LibraryService.INSTANCE.createPluginService(plugin)
				.createDomain();
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static WorkProductType createWorkProductType(MethodPlugin plugin,
			String name) {
		WorkProductType element = LibraryService.INSTANCE.createPluginService(
				plugin).createWorkProductType();
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static CustomCategory createCustomCategory(MethodPlugin plugin,
			CustomCategory parent, String name) {
		CustomCategory element = LibraryService.INSTANCE.createPluginService(
				plugin).createCustomCategory(parent);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static ContentPackage createContentPackage(String name) {

		return createContentPackage(null, name);
	}

	public static ContentPackage createContentPackage(MethodPlugin plugin,
			String name) {
		if (plugin == null) {
			plugin = createMethodPlugin("test plugin");
		}
		ContentPackage element = LibraryService.INSTANCE.createPluginService(
				plugin).createContentPackage(null);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Task createTask(ContentPackage pkg, String name) {
		MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
		Task element = LibraryService.INSTANCE.createPluginService(plugin)
				.createTask(pkg);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Role createRole(ContentPackage pkg, String name) {
		MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
		Role element = LibraryService.INSTANCE.createPluginService(plugin)
				.createRole(pkg);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Artifact createArtifact(ContentPackage pkg, String name) {
		MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
		Artifact element = LibraryService.INSTANCE.createPluginService(plugin)
				.createArtifact(pkg);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Guidance createGuidance(ContentPackage pkg, String name) {
		MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
		Guidance element = LibraryService.INSTANCE.createPluginService(plugin)
				.createGuidance(pkg);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Checklist createChecklist(ContentPackage pkg, String name) {
		MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
		Checklist element = LibraryService.INSTANCE.createPluginService(plugin)
				.createChecklist(pkg);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static SupportingMaterial createSupportingMaterial(
			ContentPackage pkg, String name) {
		MethodPlugin plugin = LibraryUtil.getMethodPlugin(pkg);
		SupportingMaterial element = LibraryService.INSTANCE
				.createPluginService(plugin).createSupportingMaterial(pkg);
		element.setName(name);
		element.setGuid(EcoreUtil.generateUUID());

		return element;
	}

	public static Section createSection(ContentDescription desc, String name,
			String content) {
		Section e = UmaFactory.eINSTANCE.createSection();
		e.setSectionName(name);
		e.setSectionDescription(content);
		e.setGuid(EcoreUtil.generateUUID());

		desc.getSections().add(e);

		return e;
	}

	public static CapabilityPattern createCapabilityPattern(
			MethodPlugin plugin, String name, MethodConfiguration defaultConfig) {
		CapabilityPattern cp = LibraryService.INSTANCE.createPluginService(
				plugin).createCapabilityPattern(defaultConfig);
		cp.setName(name);
		((ProcessComponent) cp.eContainer()).setName(name);

		saveLibrary();

		return cp;
	}

	public static Activity createActivity(MethodPlugin plugin,
			Activity superActivity, String name, Task[] tasks) {
		PluginService svc = LibraryService.INSTANCE.createPluginService(plugin);
		Activity act = svc.createActivity(superActivity);
		act.setName(name);
		saveLibrary();

		if (tasks != null && tasks.length > 0) {
			for (int i = 0; i < tasks.length; i++) {
				svc.addTaskToActivity(act, tasks[i]);
			}
			saveLibrary();
		}

		return act;
	}

	public static void setExtends(VariabilityElement element,
			VariabilityElement base) {
		element.setVariabilityBasedOnElement(base);
		element.setVariabilityType(VariabilityType.EXTENDS);
	}

	public static void setContributes(VariabilityElement element,
			VariabilityElement base) {
		element.setVariabilityBasedOnElement(base);
		element.setVariabilityType(VariabilityType.CONTRIBUTES);
	}

	public static void setReplaces(VariabilityElement element,
			VariabilityElement base) {
		element.setVariabilityBasedOnElement(base);
		element.setVariabilityType(VariabilityType.REPLACES);
	}
}
