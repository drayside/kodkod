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
    private final int hashCode;
    
    /**  
     * Constructs a new binary formula:  left op right
     * 
     * @effects this.left' = left && this.right' = right &&  this.op' = op
     * @throws NullPointerException - left = null || right = null || op = null
     */
    BinaryFormula(Formula left, Operator op, Formula right) {
        if (left == null  || right == null || op == null) throw new NullPointerException("null argument");
        this.left = left;
        this.right = right;
        this.op = op;
        this.hashCode = op.hashCode() + left.hashCode() + right.hashCode();
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
     * Returns true of o is a BinaryFormula with the
     * same tree structure as this.
     * @return o.op.equals(this.op) && o.left.equals(this.left) && o.right.equals(this.right) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof BinaryFormula)) return false;
    	BinaryFormula that = (BinaryFormula)o;
    	return op.equals(that.op) &&
    		left.equals(that.left) &&
    		right.equals(that.right);
    }
    
    public int hashCode() {
    	return hashCode;
    }

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
