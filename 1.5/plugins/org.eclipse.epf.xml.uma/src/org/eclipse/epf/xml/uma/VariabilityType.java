/**
 * <copyright>
 * </copyright>
 *
 * $Id: VariabilityType.java,v 1.1 2008/01/15 08:52:07 jtham Exp $
 */
package org.eclipse.epf.xml.uma;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.AbstractEnumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Variability Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * <!-- begin-model-doc -->
 * An Enumeration used for values for instances of Variability Element's attribute variabilityType.  It defines the nature of how a Variability Element extends another Variability Element. See enumeration literals for definitions for each type.
 * <!-- end-model-doc -->
 * @see org.eclipse.epf.xml.uma.UmaPackage#getVariabilityType()
 * @model
 * @generated
 */
public final class VariabilityType extends AbstractEnumerator {
	/**
	 * The '<em><b>Na</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Na</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #NA_LITERAL
	 * @model name="na"
	 * @generated
	 * @ordered
	 */
	public static final int NA = 0;

	/**
	 * The '<em><b>Contributes</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Contributes</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #CONTRIBUTES_LITERAL
	 * @model name="contributes"
	 * @generated
	 * @ordered
	 */
	public static final int CONTRIBUTES = 1;

	/**
	 * The '<em><b>Extends</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Extends</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #EXTENDS_LITERAL
	 * @model name="extends"
	 * @generated
	 * @ordered
	 */
	public static final int EXTENDS = 2;

	/**
	 * The '<em><b>Replaces</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Replaces</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #REPLACES_LITERAL
	 * @model name="replaces"
	 * @generated
	 * @ordered
	 */
	public static final int REPLACES = 3;

	/**
	 * The '<em><b>Local Contribution</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Local Contribution</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #LOCAL_CONTRIBUTION_LITERAL
	 * @model name="localContribution"
	 * @generated
	 * @ordered
	 */
	public static final int LOCAL_CONTRIBUTION = 4;

	/**
	 * The '<em><b>Local Replacement</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Local Replacement</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #LOCAL_REPLACEMENT_LITERAL
	 * @model name="localReplacement"
	 * @generated
	 * @ordered
	 */
	public static final int LOCAL_REPLACEMENT = 5;

	/**
	 * The '<em><b>Extends Replaces</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Extends Replaces</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #EXTENDS_REPLACES_LITERAL
	 * @model name="extendsReplaces"
	 * @generated
	 * @ordered
	 */
	public static final int EXTENDS_REPLACES = 6;

	/**
	 * The '<em><b>Na</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #NA
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType NA_LITERAL = new VariabilityType(NA, "na", "na");

	/**
	 * The '<em><b>Contributes</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONTRIBUTES
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType CONTRIBUTES_LITERAL = new VariabilityType(CONTRIBUTES, "contributes", "contributes");

	/**
	 * The '<em><b>Extends</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EXTENDS
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType EXTENDS_LITERAL = new VariabilityType(EXTENDS, "extends", "extends");

	/**
	 * The '<em><b>Replaces</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #REPLACES
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType REPLACES_LITERAL = new VariabilityType(REPLACES, "replaces", "replaces");

	/**
	 * The '<em><b>Local Contribution</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LOCAL_CONTRIBUTION
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType LOCAL_CONTRIBUTION_LITERAL = new VariabilityType(LOCAL_CONTRIBUTION, "localContribution", "localContribution");

	/**
	 * The '<em><b>Local Replacement</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LOCAL_REPLACEMENT
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType LOCAL_REPLACEMENT_LITERAL = new VariabilityType(LOCAL_REPLACEMENT, "localReplacement", "localReplacement");

	/**
	 * The '<em><b>Extends Replaces</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EXTENDS_REPLACES
	 * @generated
	 * @ordered
	 */
	public static final VariabilityType EXTENDS_REPLACES_LITERAL = new VariabilityType(EXTENDS_REPLACES, "extendsReplaces", "extendsReplaces");

	/**
	 * An array of all the '<em><b>Variability Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final VariabilityType[] VALUES_ARRAY =
		new VariabilityType[] {
			NA_LITERAL,
			CONTRIBUTES_LITERAL,
			EXTENDS_LITERAL,
			REPLACES_LITERAL,
			LOCAL_CONTRIBUTION_LITERAL,
			LOCAL_REPLACEMENT_LITERAL,
			EXTENDS_REPLACES_LITERAL,
		};

	/**
	 * A public read-only list of all the '<em><b>Variability Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Variability Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static VariabilityType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			VariabilityType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Variability Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static VariabilityType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			VariabilityType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Variability Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static VariabilityType get(int value) {
		switch (value) {
			case NA: return NA_LITERAL;
			case CONTRIBUTES: return CONTRIBUTES_LITERAL;
			case EXTENDS: return EXTENDS_LITERAL;
			case REPLACES: return REPLACES_LITERAL;
			case LOCAL_CONTRIBUTION: return LOCAL_CONTRIBUTION_LITERAL;
			case LOCAL_REPLACEMENT: return LOCAL_REPLACEMENT_LITERAL;
			case EXTENDS_REPLACES: return EXTENDS_REPLACES_LITERAL;
		}
		return null;
	}

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private VariabilityType(int value, String name, String literal) {
		super(value, name, literal);
	}

} //VariabilityType
