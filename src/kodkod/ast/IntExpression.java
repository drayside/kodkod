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
	 * Returns an IntExpression that represents the sum of this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.PLUS, intexpr).
	 * @return {e: IntExpression | [[e]] = this + intexpr }
	 */
	public final IntExpression plus(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.PLUS, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the difference between this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.MINUS, intexpr).
	 * @return {e: IntExpression | [[e]] = this - intexpr }
	 */
	public final IntExpression minus(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.MINUS, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the product of this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.MULTIPLY, intexpr).
	 * @return {e: IntExpression | [[e]] = this * intexpr }
	 */
	public final IntExpression multiply(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.MULTIPLY, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the ratio of this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.DIVIDE, intexpr).
	 * @return {e: IntExpression | [[e]] = this / intexpr }
	 */
	public final IntExpression divide(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.DIVIDE, intexpr);
	}
	
	/**
	 * Returns an expression that combines this and the given integer expression using the
	 * specified operatior.
	 * @return {e: IntExpression | [[e]] = this op intexpr }
	 */
	public IntExpression compose(BinaryIntExpression.Operator op, IntExpression intexpr) {
		if (op==null || intexpr==null)
			throw new NullPointerException();
		return new BinaryIntExpression(this, op, intexpr);	
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
