/**
 * <copyright>
 * </copyright>
 *
 * $Id: PseudoStateKind.java,v 1.1 2008/01/15 08:52:03 jtham Exp $
 */
package org.eclipse.epf.uma;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.AbstractEnumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Pseudo State Kind</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see org.eclipse.epf.uma.UmaPackage#getPseudoStateKind()
 * @model
 * @generated
 */
public final class PseudoStateKind extends AbstractEnumerator {
	/**
	 * The '<em><b>Initial</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Initial</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #INITIAL_LITERAL
	 * @model name="initial"
	 * @generated
	 * @ordered
	 */
	public static final int INITIAL = 0;

	/**
	 * The '<em><b>Join</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Join</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #JOIN_LITERAL
	 * @model name="join"
	 * @generated
	 * @ordered
	 */
	public static final int JOIN = 1;

	/**
	 * The '<em><b>Fork</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Fork</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FORK_LITERAL
	 * @model name="fork"
	 * @generated
	 * @ordered
	 */
	public static final int FORK = 2;

	/**
	 * The '<em><b>Junction</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Junction</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #JUNCTION_LITERAL
	 * @model name="junction"
	 * @generated
	 * @ordered
	 */
	public static final int JUNCTION = 3;

	/**
	 * The '<em><b>Choice</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Choice</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #CHOICE_LITERAL
	 * @model name="choice"
	 * @generated
	 * @ordered
	 */
	public static final int CHOICE = 4;

	/**
	 * The '<em><b>Entry Point</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Entry Point</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #ENTRY_POINT_LITERAL
	 * @model name="entryPoint"
	 * @generated
	 * @ordered
	 */
	public static final int ENTRY_POINT = 5;

	/**
	 * The '<em><b>Exit Point</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Exit Point</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #EXIT_POINT_LITERAL
	 * @model name="exitPoint"
	 * @generated
	 * @ordered
	 */
	public static final int EXIT_POINT = 6;

	/**
	 * The '<em><b>Terminate</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Terminate</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #TERMINATE_LITERAL
	 * @model name="terminate"
	 * @generated
	 * @ordered
	 */
	public static final int TERMINATE = 7;

	/**
	 * The '<em><b>Initial</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INITIAL
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind INITIAL_LITERAL = new PseudoStateKind(
			INITIAL, "initial", "initial"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Join</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #JOIN
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind JOIN_LITERAL = new PseudoStateKind(
			JOIN, "join", "join"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Fork</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FORK
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind FORK_LITERAL = new PseudoStateKind(
			FORK, "fork", "fork"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Junction</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #JUNCTION
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind JUNCTION_LITERAL = new PseudoStateKind(
			JUNCTION, "junction", "junction"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Choice</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CHOICE
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind CHOICE_LITERAL = new PseudoStateKind(
			CHOICE, "choice", "choice"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Entry Point</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ENTRY_POINT
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind ENTRY_POINT_LITERAL = new PseudoStateKind(
			ENTRY_POINT, "entryPoint", "entryPoint"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Exit Point</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EXIT_POINT
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind EXIT_POINT_LITERAL = new PseudoStateKind(
			EXIT_POINT, "exitPoint", "exitPoint"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * The '<em><b>Terminate</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TERMINATE
	 * @generated
	 * @ordered
	 */
	public static final PseudoStateKind TERMINATE_LITERAL = new PseudoStateKind(
			TERMINATE, "terminate", "terminate"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * An array of all the '<em><b>Pseudo State Kind</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final PseudoStateKind[] VALUES_ARRAY = new PseudoStateKind[] {
			INITIAL_LITERAL, JOIN_LITERAL, FORK_LITERAL, JUNCTION_LITERAL,
			CHOICE_LITERAL, ENTRY_POINT_LITERAL, EXIT_POINT_LITERAL,
			TERMINATE_LITERAL, };

	/**
	 * A public read-only list of all the '<em><b>Pseudo State Kind</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List VALUES = Collections.unmodifiableList(Arrays
			.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Pseudo State Kind</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static PseudoStateKind get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			PseudoStateKind result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Pseudo State Kind</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static PseudoStateKind getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			PseudoStateKind result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Pseudo State Kind</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static PseudoStateKind get(int value) {
		switch (value) {
		case INITIAL:
			return INITIAL_LITERAL;
		case JOIN:
			return JOIN_LITERAL;
		case FORK:
			return FORK_LITERAL;
		case JUNCTION:
			return JUNCTION_LITERAL;
		case CHOICE:
			return CHOICE_LITERAL;
		case ENTRY_POINT:
			return ENTRY_POINT_LITERAL;
		case EXIT_POINT:
			return EXIT_POINT_LITERAL;
		case TERMINATE:
			return TERMINATE_LITERAL;
		}
		return null;
	}

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private PseudoStateKind(int value, String name, String literal) {
		super(value, name, literal);
	}

} //PseudoStateKind
