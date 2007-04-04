/**
 * 
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * An int expression whose value depends on the truth of a condition.
 * 
 * @specfield condition: Formula
 * @specfield thenExpr: IntExpression
 * @specfield elseExpr: IntExpression
 * @invariant children = condition + thenExpr + elseExpr
 * @author Emina Torlak
 */
public final class IfIntExpression extends IntExpression {
	private final Formula condition;
	private final IntExpression thenExpr, elseExpr;

	/**
	 * @effects this.condition' = condition && this.thenExpr' = thenExpr &&
	 *          this.elseExpr' = elseExpr
	 */
	IfIntExpression(Formula condition, IntExpression thenExpr,
			IntExpression elseExpr) {
		this.condition = condition;
		this.thenExpr = thenExpr;
		this.elseExpr = elseExpr;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#accept(kodkod.ast.visitor.ReturnVisitor)
	 */
	@Override
	public <E, F, D, I> I accept(ReturnVisitor<E, F, D, I> visitor) {
		return visitor.visit(this);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#accept(kodkod.ast.visitor.VoidVisitor)
	 */
	@Override
	public void accept(VoidVisitor visitor) {
		visitor.visit(this);
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
	public IntExpression thenExpr() {
		return thenExpr;
	}

	/**
	 * Returns the else-expression.
	 * @return this.elseExpr
	 */
	public IntExpression elseExpr() {
		return elseExpr;
	}

	/**
	 * Returns the string representation of this int expression.
	 * @return string representation of this int expression
	 */
	public String toString() {
		return "(if " + condition + " then " + thenExpr + " else " + elseExpr + ")";
	}

}
