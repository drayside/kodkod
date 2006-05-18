/*
 * UnaryExpression.java
 * Created on Jul 1, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/** 
 * Represents a unary {@link kodkod.ast.Expression expression}.
 * 
 * @specfield expression: Expression
 * @specfield op: Operator
 * @invariant children = expression
 * @author Emina Torlak 
 */
public final class UnaryExpression extends Expression {
    private final Expression expression;
    private final Operator op;
    private final int arity;
    private final int hashCode;
    
    /**  
     * Constructs a new unary expression: op expression
     * 
     * @effects this.expression' = expression && this.op' = op
     * @throws NullPointerException - expression = null || op = null
     * @throws IllegalArgumentException - op in {TRANSPOSE, CLOSURE, REFLEXIVE_CLOSURE} && child.arity != 2
     */
    UnaryExpression(Operator op, Expression child) {
        if (!op.applicable(child.arity())) {
            throw new IllegalArgumentException("Invalid arity: " + child + "::" + child.arity());
        }
        this.expression = child;
        this.op = op;
        this.arity = op.arity(child.arity());
        this.hashCode = op.hashCode() + expression.hashCode();
    }

    /**
     * Returns the arity of this expression.
     * @return this.arity
     * @see kodkod.ast.Expression#arity()
     */
    public int arity() {
        return arity;
    }
    
    /**
     * Returns this.expression.
     * @return this.expression
     */
    public Expression expression() {return expression;}
    
    /**
     * Returns this.op.
     * @return this.op
     */
    public Operator op() {return op;}
    
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public <E, F, D, I> E accept(ReturnVisitor<E, F, D, I> visitor) {
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
     * Returns true of o is a UnaryExpression with the
     * same tree structure as this.
     * @return o.op.equals(this.op) && o.expression.equals(this.expression) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof UnaryExpression)) return false;
    	UnaryExpression that = (UnaryExpression)o;
    	return op.equals(that.op) &&
    		expression.equals(that.expression);
    }
    
    public int hashCode() {
    	return hashCode;
    }

    public String toString() {
        return op.toString() + expression.toString();
    }

    /**
     * Represents a unary expression operator.
     */
    public static enum Operator  {
        
        TRANSPOSE { public String toString() { return "~";}},
        CLOSURE { public String toString() { return "^";}},
        REFLEXIVE_CLOSURE { public String toString() { return "*";}};
        
        /**
         * @return true if this operator can be applied to an expression with the given arity.
         */
        boolean applicable(int childArity) { return childArity == 2; }
        
        /**
         * @return the arity of the expression that results from applying
         * this operator to an expression with the given arity.
         */
        int arity(int childArity) { return 2; }
        
    }
    

}
