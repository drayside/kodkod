/**
 * Multiplicity.java
 * Created on 12:31:53 PM
 */
package kodkod.ast;

/**
 * Represents the multiplicity of an expression
 * in a {@link kodkod.ast.MultiplicityFormula} or
 * the multiplicity of a variable in a {@link kodkod.ast.Decl }.
 * 
 * @author Emina Torlak
 */
public enum Multiplicity {
	/** <tt>no expr</tt>: expr contains no elements.  The 'no' multiplicity can only be used in a multiplicity formula. */
    NO { public String toString() { return "no"; }},
    /** <tt>lone expr</tt>: expr contains at most one element. */
    LONE { public String toString() { return "lone"; }},
    /** <tt>one expr</tt>: expr contains exactly one element. */
    ONE { public String toString() { return "one"; }},
    /** <tt>some expr</tt>: expr contains at least one element. */
    SOME { public String toString() { return "some"; }},
    /** <tt>v: set expr</tt>: v is a subset of expr.  The 'set' multiplicity can only be used in a declaration. */
    SET { public String toString() { return "set"; }}
}