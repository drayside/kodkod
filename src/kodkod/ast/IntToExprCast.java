/**
 * 
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents the conversion from an {@link kodkod.ast.IntExpression int expression }
 * to an {@link kodkod.ast.Expression expression}.  The meaning of the resulting 
 * expression is a singleton set containing the atom that represents the integer 
 * given by the wrapped int expression.
 * @specfield intExpr: IntExpression
 * @invariant children = intExpr
 * @invariant arity = 1
 * @author Emina Torlak
 */
public final class IntToExprCast extends Expression {
	private final IntExpression intExpr;
	/**
	 * Constructs a new IntToExprCast.
	 * @effects this.intexpr' = intExpr
	 */
	IntToExprCast(IntExpression intexpr) {
		this.intExpr = intexpr;
	}

	/**
	 * Returns 1.
	 * @return 1
	 */
	@Override
	public int arity() {
		return 1;
	}

	/**
	 * Returns this.intExpr.
	 * @return this.intExpr
	 */
	public IntExpression intExpr() {
		return intExpr;
	}
		
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.Expression#accept(kodkod.ast.visitor.ReturnVisitor)
	 */
	@Override
	public <E, F, D, I> E accept(ReturnVisitor<E, F, D, I> visitor) {
		return visitor.visit(this);
	}

	/**
	 * {@inheritDoc}
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
		return "((Expression)" + intExpr + ")";
	}
	
}
