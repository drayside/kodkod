/*
 * BinaryFormula.java
 * Created on Jul 1, 2005
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;



/** 
 * Represents a binary {@link kodkod.ast.Formula formula}.
 * 
 * @specfield left: Formula
 * @specfield right: Formula
 * @specfield op: AbstractOperator
 * @invariant children = left + right
 * @author Emina Torlak 
 */
public final class BinaryFormula extends Formula {
	
    private final Formula left;
    private final Formula right;
    private final Operator op;
    
    /**  
     * Constructs a new binary formula:  left op right
     * 
     * @effects this.left' = left && this.right' = right &&  this.op' = op
     * @throws NullPointerException - left = null || right = null || op = null
     */
    BinaryFormula(Formula left, Operator op, Formula right) {
        this.left = left;
        this.right = right;
        this.op = op;
    }    
    
    /**
     * Returns the left child of this.
     * @return this.left
     */
    public Formula left() {return left;}
    
    /**
     * Returns the right child of this.
     * @return this.right
     */
    public Formula right() {return right;}
    
    /**
     * Returns the operator of this.
     * @return this.op
     */
    public Operator op() {return op;}
 
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public <E, F, D, I> F accept(ReturnVisitor<E, F, D, I> visitor) {
        return visitor.visit(this);
    }
    
    
    /**
     * Accepts the given visitor.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
     */
    public void accept(VoidVisitor visitor) {
        visitor.visit(this);
    }
    
    /**
	 * Returns the string representation of this formula.
	 * @return string representation of this formula
	 */
    public String toString() {
        return "(" + left + " " + op + " " + right + ")";
    }
    
    /**
     * Represents a binary formula operator. 
     */
    public static enum Operator {
        /** Logical AND operator. */      
        AND { public String toString() { return "&&"; }},
        /** Logical OR operator. */      
        OR { public String toString() { return "||"; }},
        /** Logical XOR operator. */      
        XOR { public String toString() { return "xor"; } },
        /** Logical implication operator. */      
        IMPLIES { public String toString() { return "=>"; }},
        /** Logical bi-implication operator. */
        IFF { public String toString() { return "<=>"; }}
    }

   

}
