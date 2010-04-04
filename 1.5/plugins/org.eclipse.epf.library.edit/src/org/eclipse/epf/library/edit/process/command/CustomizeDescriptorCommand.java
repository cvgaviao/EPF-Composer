package org.eclipse.epf.library.edit.process.command;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.command.AbstractCommand;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.epf.library.edit.command.IResourceAwareCommand;
import org.eclipse.epf.library.edit.process.BreakdownElementWrapperItemProvider;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.MethodElement;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.TaskDescriptor;
import org.eclipse.epf.uma.UmaFactory;
import org.eclipse.epf.uma.UmaPackage;


public class CustomizeDescriptorCommand extends AbstractCommand implements
		IResourceAwareCommand {

	private Process proc;
	private BreakdownElementWrapperItemProvider wrapper;
	private Descriptor greenParent;
	private boolean debug = false;


	protected List createdActivities;
	
	public Activity superActivity;

	public CustomizeDescriptorCommand(BreakdownElementWrapperItemProvider wrapper) {
		this.wrapper = wrapper;
		proc = (Process) wrapper.getTopItem();
		Object obj = TngUtil.unwrap(wrapper);
		if (obj instanceof Descriptor) {
			greenParent = (Descriptor) obj;
		}		
	}

	public BreakdownElementWrapperItemProvider getWrapper() {
		return wrapper;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.library.edit.command.IResourceAwareCommand#getModifiedResources()
	 */
	public Collection getModifiedResources() {
		if (proc.eResource() != null) {
			return Collections.singletonList(proc.eResource());
		}
		return Collections.EMPTY_LIST;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.AbstractCommand#prepare()
	 */
	protected boolean prepare() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#execute()
	 */
	static int cc = 0;
	public void execute() {
		if (greenParent == null || ! (greenParent instanceof TaskDescriptor)) {
			return;
		}		
		Object parentObj = wrapper.getParent(null);
		Activity parentAct = parentObj instanceof Activity ? (Activity) parentObj : null;
		if (parentAct == null) {
			return;
		}
		if (proc != ProcessUtil.getProcess(parentAct)) {
			return;
		}
		
		Descriptor des = UmaFactory.eINSTANCE.createTaskDescriptor();
		
		updateFromGreenParent(greenParent, des);
		
		parentAct.getBreakdownElements().add(des);
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		propUtil.setGreenParent(des, greenParent.getGuid());
	}

	private boolean isAttToCopy(EAttribute attribute) {
		if (!attribute.isChangeable()) {
			return false;	
		}
		if (attribute.isDerived()) {
			return false;
		}
		if (attribute == UmaPackage.eINSTANCE.getMethodElement_Guid()) {
			return false;
		}
		if (attribute == UmaPackage.eINSTANCE.getNamedElement_Name()) {
			return false;
		}
		return true;
	}
	

	private boolean isRefToCopy(EReference ref) {
		if (!ref.isChangeable()) {
			return false;	
		}
		if (ref.isDerived()) {
			return false;
		}
		if (ref == UmaPackage.eINSTANCE.getDescribableElement_Presentation()) {
			return false;
		}
		return true;
	}
	
	private void copyAttributes(MethodElement source, MethodElement target) {
		if (source == null || target == null || source.eClass() != target.eClass()) {
			return;
		}				
		Collection<EAttribute> attributes = source.eClass().getEAllAttributes();		
		for (EAttribute attribute : attributes) {
			if (isAttToCopy(attribute)) {
				Object value = source.eGet(attribute);
				if (value != null) {
					target.eSet(attribute, value);
				}
			}
		}
	}
	
	private void copyReferences(MethodElement source, MethodElement target) {
		if (source == null || target == null || source.eClass() != target.eClass()) {
			return;
		}	
		Collection<EReference> references = source.eClass().getEAllReferences();
		
		if (debug) {
			System.out.println("\nLD> source: " + source); //$NON-NLS-1$
		}
		
		for (EReference reference : references) {
			if (debug) {				
				if (! reference.isContainment()) {
					System.out.println("LD> reference: " + reference.getName() + ", type : " + reference.getEType()); //$NON-NLS-1$
				}				
			}			
			
			if (!reference.isChangeable() || reference.isDerived()) {
				continue;
			}
			Object value = source.eGet(reference);
			if (value == null) {
				continue;
			}

			if (reference.isMany()) {
				List valueList = (List) value;
				if (! valueList.isEmpty()) {
					EList copyList = (EList) target.eGet(reference);
					copyList.clear();
					copyList.addAll(valueList);
				}
			} else {
				target.eSet(reference, value);
			}
		}
	}
	
	public void updateFromGreenParent(Descriptor greenParent, Descriptor child) {
		child.setName(greenParent.getName());
		copyAttributes(greenParent, child);
		copyAttributes(greenParent.getPresentation(), child.getPresentation());
		
		if (debug) {
			child.setName(greenParent.getName() + "_n-modified");		//$NON-NLS-1$
			child.setPresentationName(greenParent.getPresentationName() + "_p-modified");//$NON-NLS-1$
		}
		
		copyReferences(greenParent, child);
		copyReferences(greenParent.getPresentation(), child.getPresentation());	
	}	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.Command#redo()
	 */
	public void redo() {
		
		execute();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.AbstractCommand#undo()
	 */
	public void undo() {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.emf.common.command.AbstractCommand#getResult()
	 */
	public Collection getResult() {
		return super.getResult();
	}
}
