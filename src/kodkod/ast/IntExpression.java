/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * A Node whose value is an integer
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
	 * Returns an integer expression that is the sum of all
	 * values that this integer expression can take given the
	 * provided declarations.
	 * @return {e: IntExpression | [[e]] = sum decls | this }
	 */
	public final IntExpression sum(Decls decls) {
		return new SumExpression(decls, this);
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
	 * Returns an IntExpression that represents the quotient of the division
	 * between this and the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.DIVIDE, intexpr).
	 * @return {e: IntExpression | [[e]] = this / intexpr }
	 */
	public final IntExpression divide(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.DIVIDE, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the remainder of the division
	 * between this and the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.MODULO, intexpr).
	 * @return {e: IntExpression | [[e]] = this % intexpr }
	 */
	public final IntExpression modulo(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.MODULO, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the bitwise AND of this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.AND, intexpr).
	 * @return {e: IntExpression | [[e]] = this & intexpr }
	 */
	public final IntExpression and(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.AND, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the bitwise OR of this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.OR, intexpr).
	 * @return {e: IntExpression | [[e]] = this | intexpr }
	 */
	public final IntExpression or(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.OR, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the bitwise XOR of this and
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.XOR, intexpr).
	 * @return {e: IntExpression | [[e]] = this & intexpr }
	 */
	public final IntExpression xor(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.XOR, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the left shift of this by
	 * the given int node.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.SHL, intexpr).
	 * @return {e: IntExpression | [[e]] = this << intexpr }
	 */
	public final IntExpression shl(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.SHL, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the right shift of this and
	 * the given int node, with zero extension.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.SHR, intexpr).
	 * @return {e: IntExpression | [[e]] = this >>> intexpr }
	 */
	public final IntExpression shr(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.SHR, intexpr);
	}
	
	/**
	 * Returns an IntExpression that represents the right shift of this and
	 * the given int node, with sign extension.  The effect of this method is the same as calling
	 * this.compose(BinaryIntExpression.Operator.SHA, intexpr).
	 * @return {e: IntExpression | [[e]] = this >> intexpr }
	 */
	public final IntExpression sha(IntExpression intexpr) {
		return compose(BinaryIntExpression.Operator.SHA, intexpr);
	}
	
	/**
	 * Returns an expression that combines this and the given integer expression using the
	 * specified operator.
	 * @return {e: IntExpression | [[e]] = this op intexpr }
	 */
	public final IntExpression compose(BinaryIntExpression.Operator op, IntExpression intexpr) {
		if (op==null || intexpr==null)
			throw new NullPointerException();
		return new BinaryIntExpression(this, op, intexpr);	
	}
	
	/**
	 * Returns an IntExpression that represents the negation of this int expression.
	 * The effect of this method is the same as calling this.apply(UnaryIntExpression.Operator.MINUS).
	 * @return {e: IntExpression | [[e]] = -[[e]] }
	 */
	public final IntExpression negate() {
		return apply(UnaryIntExpression.Operator.MINUS);
	}
	
	/**
	 * Returns an IntExpression that represents the bitwise negation of this int expression.
	 * The effect of this method is the same as calling this.apply(UnaryIntExpression.Operator.NOT).
	 * @return {e: IntExpression | [[e]] = ~[[e]] }
	 */
	public final IntExpression not() {
		return apply(UnaryIntExpression.Operator.NOT);
	}
	
	/**
	 * Returns an IntExpression that represents the absolute value of this int expression.
	 * The effect of this method is the same as calling this.apply(UnaryIntExpression.Operator.ABS).
	 * @return {e: IntExpression | [[e]] =  abs([[e]]) }
	 */
	public final IntExpression abs() {
		return apply(UnaryIntExpression.Operator.ABS);
	}
	
	/**
	 * Returns an IntExpression that represents the sign of this int expression.
	 * The effect of this method is the same as calling this.apply(UnaryIntExpression.Operator.SGN).
	 * @return {e: IntExpression | [[e]] =  signum([[e]]) }
	 */
	public final IntExpression signum() {
		return apply(UnaryIntExpression.Operator.SGN);
	}
	
	/**
	 * Returns an expression that represents the application of the given unary
	 * operator to this integer expression.
	 * @return {e: IntExpression | [[e]] = op ( [[this]] ) }
	 */
	public final IntExpression apply(UnaryIntExpression.Operator op) {
		if (op==null) throw new NullPointerException();
		return new UnaryIntExpression(op, this);
	}
	
	/**
	 * Returns an expression whose meaning is the singleton set containing the atom 
	 * that represents the integer given by this integer expression.
	 * The effect of this method is the same as calling this.cast(IntToExprCast.Operator.INTCAST).
	 * @return an expression whose meaning is the singleton set containing the atom 
	 * that represents the integer given by this integer expression.
	 */
	public final Expression toExpression() {
		return cast(IntToExprCast.Operator.INTCAST);
	}
	
	/**
	 * Returns an expression whose meaning is the set containing the atoms 
	 * that represent the powers of 2 (bits) present in this integer expression.
	 * The effect of this method is the same as calling this.cast(IntToExprCast.Operator.BITSETCAST).
	 * @return an expression whose meaning is the set containing the atoms 
	 * that represent the powers of 2 (bits) present in this integer expression.
	 */
	public final Expression toBitset() { 
		return cast(IntToExprCast.Operator.BITSETCAST);
	}
	
	/**
	 * Returns an expression that is the relational representation of this
	 * integer expression specified by the given operator.
	 * @return an expression that is the relational representation of this
	 * integer expression specified by the given operator.
	 */
	public final Expression cast(IntToExprCast.Operator op) { 
		if (op==null) throw new NullPointerException();
		return new IntToExprCast(this, op);
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
