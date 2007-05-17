/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
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
 * A unary integer expression, e.g. -x.
 * @specfield expression: IntExpression
 * @specfield op: Operator
 * @invariant children = expression
 * @author Emina Torlak
 */
public final class UnaryIntExpression extends IntExpression {
	private final Operator op;
	private final IntExpression expression;
	
	/**
	 * Constructs a new unary int formula: op expression
	 * @effects this.op' = op && this.expression' = expression
	 */
	UnaryIntExpression(Operator op, IntExpression expression) {
		this.op = op;
		this.expression = expression;
	}

	/**
	 * Returns the operator of this.
	 * @return this.op
	 */
	public Operator op() {return op;}
	
	/**
	 * Returns this.expression.
	 * @return this.expression
	 */
	public IntExpression expression() {return expression;}
	
	/**
	 * Returns the string representation of this int expression.
	 * @return string representation of this int expression
	 */
	public String toString() {
		return op.prefix() ? "(" + op + expression + ")" : op + "(" + expression + ")" ;
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
	 * Unary operators on integer expressions.
	 */
	public static enum Operator {
		/** unary negation (`-') operator */
		MINUS {
			public String toString() {
				return "-";
			}
		},
		/** bit negation (`~') operator */
		NOT {
			public String toString() {
				return "~";
			}
		}, 
		/** absolute value function */
		ABS {
			public String toString() {
				return "abs";
			}
			boolean prefix() { return false; } 
		}, 
		/** signum function */
		SGN {
			public String toString() {
				return "sgn";
			}
			boolean prefix() { return false; }
		};
		/**
		 * Returns true if this is a prefix operator.
		 * Otherwise returns false (i.e. if the application
		 * of the operator should be displayed as a function application)
		 * @return true if this is a prefix operator.
		 */
		boolean prefix() { return true; }
	}
}
