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
