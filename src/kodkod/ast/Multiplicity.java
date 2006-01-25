/**
 * Multiplicity.java
 * Created on 12:31:53 PM
 */
package kodkod.ast;

/**
 * Represents the multiplicity of an expression,
 * or the target multiplicity of a relation.
 * 
 * @author Emina Torlak
 */
public enum Multiplicity {
    NO { public String toString() { return "no"; }},
    LONE { public String toString() { return "lone"; }},
    ONE { public String toString() { return "one"; }},
    SOME { public String toString() { return "some"; }},
    SET { public String toString() { return ""; }}
}