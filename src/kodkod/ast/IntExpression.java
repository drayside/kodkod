/**
 * 
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a Node whose value is an integer
 * rather than a relational expression.
 * 
 * @author Emina Torlak
 */
public abstract class IntExpression implements Node {

	/**
	 * Constructs an IntExpression.
	 */
	IntExpression() {}

	/**
     * Returns the sum of this and the specified int expression.  The effect
     * of this method is the same as calling this.compose(BinaryIntExpression.Operator.PLUS, intexpr).
     * @return {e : IntExpression | e = this + intexpr}
     */
	public final IntExpression plus(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.PLUS, intexpr);
	}
	
	/**
     * Returns the difference of this and the specified int expression.  The effect
     * of this method is the same as calling this.compose(BinaryIntExpression.Operator.MINUS, intexpr).
     * @return {e : IntExpression | e = this + intexpr}
     */
	public final IntExpression minus(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.MINUS, intexpr);
	}
	
	/**
     * Returns the composition of this and the specified expression, using the
     * given binary operator.
     * @return {e: IntExpression | e = this op intexpr }
     */
	public IntExpression compose(BinaryIntExpression.Operator op, IntExpression intexpr) {
		if (op==null || intexpr==null)
			throw new NullPointerException();
		return new BinaryIntExpression(this, op, intexpr);	
	}
	
	/**
	 * Returns a formula stating that the given int expression and 
	 * this have the same value.  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.EQ, intexpr).
	 * @return {f: Formula | f <=> this = other }
	 */
	public final Formula eq(IntExpression intexpr) {
		return this.compare(IntComparisonFormula.Operator.EQ, intexpr);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is less than the 
	 * value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.LT, intexpr).
	 * @return {f: Formula | f <=> this < other }
	 */
	public final Formula lt(IntExpression intexpr) {
		return this.compare(IntComparisonFormula.Operator.LT, intexpr);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is less than
	 * or equal to the  value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.LTE, intexpr).
	 * @return {f: Formula | f <=> this <= other }
	 */
	public final Formula lte(IntExpression intexpr) {
		return this.compare(IntComparisonFormula.Operator.LTE, intexpr);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is greater than the 
	 * value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.GT, intexpr).
	 * @return {f: Formula | f <=> this > other }
	 */
	public final Formula gt(IntExpression intexpr) {
		return this.compare(IntComparisonFormula.Operator.GT, intexpr);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is greater than
	 * or equal to the  value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.GTE, intexpr).
	 * @return {f: Formula | f <=> this >= other }
	 */
	public final Formula gte(IntExpression intexpr) {
		return this.compare(IntComparisonFormula.Operator.GTE, intexpr);
	}
	
	/**
	 * Returns a formula comparing this and the given integer expression using the
	 * specified operatior.
	 * @return {f: Formula | f <=> this op intexpr }
	 */
	public Formula compare(IntComparisonFormula.Operator op, IntExpression intexpr) {
		if (op==null || intexpr==null)
			throw new NullPointerException();
		return new IntComparisonFormula(this, op, intexpr);	
	}
	
	/**
	 * Converts this int expression into an expression and returns the result.
	 * @return {e: Expresssion | e.sum() = this }
	 */
	public Expression toExpression() {
		return new IntCastExpression(this);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
	 */
	public abstract <E, F, D, I> I accept(ReturnVisitor<E, F, D, I> visitor) ;

	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
	 */
	public abstract void accept(VoidVisitor visitor);

}
