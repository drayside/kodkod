/*
 * ComparisonFormula.java
 * Created on Jul 1, 2005
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;



/** 
 * Represents a comparison formula, e.g. x = y
 * 
 * @specfield left: Expression
 * @specfield right: Expression
 * @specfield op: Operator
 * @invariant children = left + right
 * @author Emina Torlak 
 */
public final class ComparisonFormula extends Formula {
    private final Expression left;
    private final Expression right;
    private final Operator op;
    
    /**  
     * Constructs a new comparison formula: left op  right
     * 
     * @effects this.left' = left && this.right' = right && this.op' = op
     * * @throws NullPointerException - left = null || right = null || op = null
     * @throws IllegalArgumentException - left.arity != right.arity
     */
    ComparisonFormula(Expression left, Operator op, Expression right) {
        if (!op.applicable(left.arity(), right.arity())) {
            throw new IllegalArgumentException(
            		"Arity mismatch: " + left + "::" + left.arity() + 
                    " and " + right + "::" + right.arity());
        }
        this.left = left;
        this.right = right;
        this.op = op;
    }

    /**
     * Returns the left child of this.
     * @return this.left
     */
    public Expression left() {return left;}
    
    /**
     * Returns the right child of this.
     * @return this.right
     */
    public Expression right() {return right;}
    
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
     * Represents a comparison operator; e.g. "in" or "=".
     */
    public static enum Operator {
        SUBSET { public String toString() { return "in"; }},
        EQUALS { public String toString() { return "="; }};
        
        /**
         * @return true if two expressions with the given arities
         * can be compared using  this operator; otherwise returns false.  This
         * method assumes that leftArity and rightArity are positive integers.
         */
        boolean applicable(int leftArity, int rightArity) { return leftArity==rightArity; }
    }

}
