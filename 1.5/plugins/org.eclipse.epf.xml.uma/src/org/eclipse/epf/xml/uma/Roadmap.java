/**
 * <copyright>
 * </copyright>
 *
 * $Id: Roadmap.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Roadmap</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * A specific type of Guidance which is only related to Activates and therefore has been added by this package to the list of Guidance Types rather than listed in the Guidance Types package.  A Roadmap represents a linear walkthrough of an Activity, typically a Process.
 * An instance of a Roadmap represents important documentation for the Activity or Process it is related to.  Often a complex Activity such as a Process can be much easier understood by providing a walkthrough with a linear thread of a typical instantiation of this Activity.  In addition to making the process practitioner understand how work in the process is being performed, a Roadmap provides additional information about how Activities and Tasks relate to each other over time.  Roadmaps are also used to show how specific aspects are distributed over a whole process providing a kind of filter on the process for this information.
 * <!-- end-model-doc -->
 *
 *
 * @see org.eclipse.epf.xml.uma.UmaPackage#getRoadmap()
 * @model extendedMetaData="name='Roadmap' kind='elementOnly'"
 * @generated
 */
public interface Roadmap extends Guidance {
} // Roadmap