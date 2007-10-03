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
 * An integer comparison formula, e.g. x = y, x <= y, etc.
 * 
 * @specfield left: IntExpression
 * @specfield right: IntExpression
 * @specfield op: Operator
 * @invariant children = left + right
 * @author Emina Torlak 
 */
public final class IntComparisonFormula extends Formula {
	private final Operator op;
	private final IntExpression left, right;
	
	/**  
	 * Constructs a new int comparison formula: left op right
	 * 
	 * @effects this.left' = left && this.right' = right && this.op' = op
	 * @throws NullPointerException - left = null || right = null || op = null
	 */
	IntComparisonFormula(final IntExpression left, final Operator op, final IntExpression right) {
		this.left = left;
		this.right = right;
		this.op = op;
	}

	/**
	 * Returns the left child of this.
	 * @return this.left
	 */
	public IntExpression left() {return left;}
	
	/**
	 * Returns the right child of this.
	 * @return this.right
	 */
	public IntExpression right() {return right;}
	
	/**
	 * Returns the operator of this.
	 * @return this.op
	 */
	public Operator op() {return op;}
	
	/**
	 * Returns the string representation of this formula.
	 * @return string representation of this formula
	 */
	public String toString() {
		return "(" + left + " " + op + " " + right + ")";
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.Formula#accept(kodkod.ast.visitor.ReturnVisitor)
	 */
	@Override
	 public <E, F, D, I> F accept(ReturnVisitor<E, F, D, I> visitor) {
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
	 * Represents a binary comarison operator:  =, < , >, <=, >=.
	 */
	public static enum Operator implements BinaryOperator<IntExpression,Formula> {
		/** `=' operator */
		EQ { 
			public String toString() { return "="; }
			/** 
			 * Returns true.
			 * @return true
			 **/
			public boolean commutative() { return true; }
		},
		/** `<' operator */
		LT {
			public String toString() { return "<"; }
			/** 
			 * Returns false.
			 * @return false
			 **/
			public boolean commutative() { return false; }
		},
		/** `<=' operator */
		LTE { 
			public String toString() { return "<="; }
			/** 
			 * Returns false.
			 * @return false
			 **/
			public boolean commutative() { return false; }
		
		},
		/** `>' operator */
		GT { 
			public String toString() { return ">"; }
			/** 
			 * Returns false.
			 * @return false
			 **/
			public boolean commutative() { return false; }
		},
		/** `>=' operator */
		GTE { 
			public String toString() { return ">="; }
			/** 
			 * Returns false.
			 * @return false
			 **/
			public boolean commutative() { return false; }
		};

		
		/**
		 * {@inheritDoc}
		 * @see kodkod.ast.BinaryOperator#apply(java.lang.Object, java.lang.Object)
		 */
		public final Formula apply(IntExpression left, IntExpression right) { 
			return new IntComparisonFormula(left, this, right);
		}
		
		/** 
		 * Returns false.
		 * @return false
		 **/
		public final boolean associative() { return false; }
	}
	
}
