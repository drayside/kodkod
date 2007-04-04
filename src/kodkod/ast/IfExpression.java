/*
 * IfExpression.java
 * Created on November 3, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;


/**
 * An expression whose value depends on the truth of a condition.
 * 
 * @specfield condition: Formula
 * @specfield thenExpr: Expression
 * @specfield elseExpr: Expression
 * @invariant children = condition + thenExpr + elseExpr
 * @author Greg Dennis (gdennis@mit.edu)
 * @author Emina Torlak
 */
public final class IfExpression extends Expression {

	private final Formula condition;
	private final Expression thenExpr, elseExpr;
	private final int arity;
	
	/**
	 * @effects this.condition' = condition && this.thenExpr' = thenExpr &&
	 *         this.elseExpr' = elseExpr
	 * @throws IllegalArgumentException - thenExpr.arity != elseExpr.arity
	 */
	IfExpression(Formula condition, Expression thenExpr, Expression elseExpr) {
		if (thenExpr.arity() != elseExpr.arity()) {
            throw new IllegalArgumentException("Arity mismatch: " +
            		thenExpr + "::" + thenExpr.arity() +  " and " +
            		elseExpr + "::" + elseExpr.arity());
        }
		this.condition = condition;
		this.thenExpr = thenExpr;
		this.elseExpr = elseExpr;
		this.arity  = thenExpr.arity();
	}
	
	/**
	 * Returns the if-condition.
	 * @return this.condition
	 */
	public Formula condition() {
		return condition;
	}
	
	/**
	 * Returns the then-expression.
	 * @return this.thenExpr
	 */
	public Expression thenExpr() {
		return thenExpr;
	}

	/**
	 * Returns the else-expression.
	 * @return this.elseExpr
	 */
	public Expression elseExpr() {
		return elseExpr;
	}
	
	/**
	 * Returns the arity of this.
	 * @return this.arity
	 */
	@Override
	public int arity() {
		return arity;
	}

	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
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
	 * Returns the string representation of this expression.
	 * @return string representation of this expression
	 */
	public String toString() {
		return "(if " + condition + " then " + thenExpr + " else " + elseExpr + ")";
	}
	
}
