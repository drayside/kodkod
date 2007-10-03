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
 * A relational {@link kodkod.ast.Expression expression} with two children.
 * 
 * @specfield left: Expression
 * @specfield right: Expression
 * @specfield op: Operator
 * @invariant children = left + right
 * @author Emina Torlak 
 */
public final class BinaryExpression extends Expression {
	
	private final Operator op;
	private final Expression left;
	private final Expression right;
	private final int arity;
	
	/**  
	 * Constructs a new binary expression: left op right
	 * 
	 * @effects this.left' = left && this.right' = right && this.op' = op
	 * @throws NullPointerException - left = null || right = null || op = null
	 * @throws IllegalArgumentException - left and right cannot be combined with the specified operator.
	 */
	BinaryExpression(final Expression left, final Operator op, final Expression right) {
		if (!op.applicable(left.arity(),right.arity())) {
			throw new IllegalArgumentException(
					"Arity mismatch: " + left + "::" + left.arity() + 
					" and " + right + "::" + right.arity());
		}
		
		this.op = op;
		this.left = left;
		this.right = right;
		this.arity = op.arity(left.arity(),right.arity());
	}
	
	/**
	 * Returns the arity of this binary expression.
	 * @return this.arity
	 * @see kodkod.ast.Expression#arity()
	 */
	public int arity() {
		return arity;
	}
	
	/**
	 * Returns the left child of this.
	 * @return this.left
	 */
	public Expression left() {return left;}
	
	/**
	 * Returns the right child of this.
	 * @return this.right
	 */
	public Expression right() {return right;}
	
	/**
	 * Returns the operator of this.
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
	 * Returns the string representation of this expression.
	 * @return string representation of this expression
	 */
	public String toString() {
		return "(" + left + " " + op + " " + right + ")";
	}
	
	/**
	 * A binary expression operator: union, difference, intersection, override, join, and product.
	 */
	public static enum Operator implements BinaryOperator<Expression,Expression> {
		/** Relational difference (-) operator. */
		DIFFERENCE { 
			/** 
			 * Returns false.
			 * @return false 
			 **/
			public boolean commutative() { return false; }
			/** 
			 * Returns false.
			 * @return false
			 **/
			public boolean associative() { return false; }
			public String toString() { return "-"; }
		},
		
		/** Relational union (+) operator. */
		UNION {
			/** 
			 * Returns true.
			 * @return true 
			 **/
			public boolean commutative() { return true; }
			/** 
			 * Returns true.
			 * @return true 
			 **/
			public boolean associative() { return true; }
			public String toString() { return "+"; }
		},
		
		/** Relational intersection (&) operator. */
		INTERSECTION { 
			/** 
			 * Returns true.
			 * @return true 
			 **/
			public boolean commutative() { return true; }
			/** 
			 * Returns true.
			 * @return true 
			 **/
			public boolean associative() { return true; }
			public String toString() { return "&"; }
		},
		
		/** Relational override (++) operator. */
		OVERRIDE { 
			/** 
			 * Returns false.
			 * @return false 
			 **/
			public boolean commutative() { return false; }
			/** 
			 * Returns true.
			 * @return true 
			 **/
			public boolean associative() { return true; }
			public String toString() { return "++"; }
		},
		
		/** Relational join (.) operator. */
		JOIN {
			/** 
			 * Returns false.
			 * @return false 
			 **/
			public boolean commutative() { return false; }
			/** 
			 * Returns false.
			 * @return false
			 **/
			public boolean associative() { return false; }
			public String toString() { return "."; }
			boolean applicable(int leftArity, int rightArity) { return leftArity + rightArity > 2; }
			int arity(int leftArity, int rightArity) { return leftArity + rightArity - 2; }
		},
		
		/** Relational product (->) operator. */
		PRODUCT {
			/** 
			 * Returns false.
			 * @return false 
			 **/
			public boolean commutative() { return false; }
			/** 
			 * Returns true.
			 * @return true 
			 **/
			public boolean associative() { return true; }
			
			/**
			 * {@inheritDoc}
			 * @see java.lang.Enum#toString()
			 */
			public String toString() { return "->"; }
			boolean applicable(int leftArity, int rightArity) { return true; }
			int arity(int leftArity, int rightArity) { return leftArity + rightArity; }
		};
		
		/**
		 * @return true if two expressions with the given arities
		 * can be combined using  this operator; otherwise returns false.  This
		 * method assumes that leftArity and rightArity are positive integers.
		 */
		boolean applicable(int leftArity, int rightArity) { return leftArity==rightArity; }
		
		/**
		 * @return the arity of the expression that results from combining 
		 * two expressions with the given arities using this operator. 
		 * This method assumes that leftArity and rightArity are compatible, 
		 * i.e. this.compatible(leftArity, rightArity).
		 */
		int arity(int leftArity, int rightArity) { return leftArity; }
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.ast.BinaryOperator#apply(java.lang.Object, java.lang.Object)
		 */
		public final Expression apply(Expression left, Expression right) { 
			return new BinaryExpression(left, this, right);
		}

	}
}
