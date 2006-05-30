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
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.EQ, intnode).
	 * @return {f: Formula | f <=> this = other }
	 */
	public final Formula eq(IntExpression intnode) {
		return this.compare(IntComparisonFormula.Operator.EQ, intnode);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is less than the 
	 * value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.LT, intnode).
	 * @return {f: Formula | f <=> this < other }
	 */
	public final Formula lt(IntExpression intnode) {
		return this.compare(IntComparisonFormula.Operator.LT, intnode);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is less than
	 * or equal to the  value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.LTE, intnode).
	 * @return {f: Formula | f <=> this <= other }
	 */
	public final Formula lte(IntExpression intnode) {
		return this.compare(IntComparisonFormula.Operator.LTE, intnode);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is greater than the 
	 * value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.GT, intnode).
	 * @return {f: Formula | f <=> this > other }
	 */
	public final Formula gt(IntExpression intnode) {
		return this.compare(IntComparisonFormula.Operator.GT, intnode);
	}
	
	/**
	 * Returns a formula stating that the value of this int expression is greater than
	 * or equal to the  value of the given int expression  The effect
     * of this method is the same as calling this.compose(IntComparisonFormula.Operator.GTE, intnode).
	 * @return {f: Formula | f <=> this >= other }
	 */
	public final Formula gte(IntExpression intnode) {
		return this.compare(IntComparisonFormula.Operator.GTE, intnode);
	}
	
	/**
	 * Returns a formula comparing this and the given integer expression using the
	 * specified operatior.
	 * @return {f: Formula | f <=> this op intnode }
	 */
	public Formula compare(IntComparisonFormula.Operator op, IntExpression intnode) {
		if (op==null || intnode==null)
			throw new NullPointerException();
		return new IntComparisonFormula(this, op, intnode);	
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
