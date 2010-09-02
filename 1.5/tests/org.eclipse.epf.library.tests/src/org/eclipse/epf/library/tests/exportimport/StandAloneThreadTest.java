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
package org.eclipse.epf.library.tests.exportimport;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.LibraryService;
import org.eclipse.epf.library.configuration.ConfigurationHelper;
import org.eclipse.epf.library.tester.LibraryTestService;
import org.eclipse.epf.services.Services;
import org.eclipse.epf.uma.MethodConfiguration;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.MethodLibrary;
import org.eclipse.epf.uma.MethodPlugin;

/**
 * JUnit tests for thread test
 * 
 * @author Weiping Lu
 * @since 1.5
 * 
 */
public class StandAloneThreadTest extends TestCase {

	private LibraryTestService service = LibraryTestService.getInstance();
	
	public StandAloneThreadTest() {
	}	
	
	public void test0001() {	
		
		LibraryPlugin p = LibraryPlugin.getDefault();
		String libPath = "C:\\Documents and Settings\\wlu\\RMC\\Method Libraries\\lib.7.5.rup";
		
		MethodLibrary lib = null;
		try {
			lib = LibraryService.getInstance().openMethodLibrary(
					Services.XMI_PERSISTENCE_TYPE, new File(libPath).toURI());
		} catch (Exception e) {
			e.printStackTrace();
		}
		MethodConfiguration config = lib.getPredefinedConfigurations().get(5);
		System.out.println("LD> config: " + config.getName() + ", " + 	Thread.currentThread());
		System.out.println("");
		
		List<Thread> threadList = new ArrayList<Thread>();
		for (MethodPlugin plugin : lib.getMethodPlugins()) {
			PluginJob job = new PluginJob(plugin, config);
			Thread th = new Thread(job);
			th.start();
			threadList.add(th);
		}
		for (Thread th : threadList) {
			try {
				th.join();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

	}		
			
	static class PluginJob implements Runnable {

		private MethodPlugin plugin;
		MethodConfiguration config;
		public PluginJob(MethodPlugin plugin, MethodConfiguration config) {
			this.plugin = plugin;
			this.config = config;
		}
		
		public void run() {
			System.out.println("LD> plugin: " + plugin.getName() + ", " + 	Thread.currentThread());
			System.out.println("");
			
			for (Iterator<EObject> iter = plugin.eAllContents(); iter.hasNext();) {
				try {
					EObject obj = (EObject) iter.next();
					if (obj instanceof MethodElement) {
						MethodElement element = (MethodElement) obj;
						boolean b = ConfigurationHelper.inConfig(element,
								config);
//						System.out
//								.println("LD> " + b + ", element: " + element);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}	
		
	}
	
}
