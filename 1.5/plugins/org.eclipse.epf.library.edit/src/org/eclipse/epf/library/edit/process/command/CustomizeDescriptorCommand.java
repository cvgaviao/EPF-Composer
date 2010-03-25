package org.eclipse.epf.library.edit.process.command;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.command.AbstractCommand;
import org.eclipse.epf.library.edit.command.IResourceAwareCommand;
import org.eclipse.epf.library.edit.process.BreakdownElementWrapperItemProvider;
import org.eclipse.epf.library.edit.util.DescriptorPropUtil;
import org.eclipse.epf.library.edit.util.ProcessUtil;
import org.eclipse.epf.library.edit.util.TngUtil;
import org.eclipse.epf.uma.Activity;
import org.eclipse.epf.uma.Descriptor;
import org.eclipse.epf.uma.Process;
import org.eclipse.epf.uma.UmaFactory;


public class CustomizeDescriptorCommand extends AbstractCommand implements
		IResourceAwareCommand {

	private Process proc;
	private BreakdownElementWrapperItemProvider wrapper;
	private Descriptor greenParent;



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
		if (greenParent == null) {
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
		
		//To do: copy some feature values of greenParent to newDes
		des.setName(greenParent.getName());
		des.setPresentationName(greenParent.getPresentationName());
		
		parentAct.getBreakdownElements().add(des);
		DescriptorPropUtil propUtil = DescriptorPropUtil.getDesciptorPropUtil();
		propUtil.setGreenParent(des, greenParent.getGuid());
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
