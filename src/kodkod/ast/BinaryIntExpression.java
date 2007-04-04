/**
 * 
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a binary integer expression, e.g. x + y.
 * @specfield left: IntExpression
 * @specfield right: IntExpression
 * @specfield op: Operator
 * @invariant children = left + right
 * @author Emina Torlak
 */
public final class BinaryIntExpression extends IntExpression {
	private final Operator op;
	private final IntExpression left, right;
	
	/**  
	 * Constructs a new binary int formula: left op right
	 * 
	 * @effects this.left' = left && this.right' = right && this.op' = op
	 * @throws NullPointerException - left = null || right = null || op = null
	 */
	public BinaryIntExpression(final IntExpression left, final Operator op, final IntExpression right) {
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
	 * Returns the string representation of this int expression.
	 * @return string representation of this int expression
	 */
	public String toString() {
		return "(" + left + " " + op + " " + right + ")";
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
	 * BinaryGate operators on integer expressions.
	 */
	public static enum Operator {
		/** `+' operator */
		PLUS {
			public String toString() {
				return "+";
			}
		},
		/** `-' operator */
		MINUS {
			public String toString() {
				return "-";
			}
		},
		/** `*' operator */
		MULTIPLY {
			public String toString() {
				return "*";
			}
		}, 
		/** `/' operator */
		DIVIDE {
			public String toString() {
				return "/";
			}
		}, 
		/** bitwise AND operator */
		AND {
			public String toString() {
				return "&";
			}
		},
		/** bitwise OR operator */
		OR {
			public String toString() {
				return "|";
			}
		}, 
		/** bitwise XOR operator */
		XOR {
			public String toString() {
				return "^";
			}
		}, 
		/** left shift operator */
		SHL {
			public String toString() {
				return "<<";
			}
		}, 
		/** right shift operator with zero extension */
		SHR {
			public String toString() {
				return ">>>";
			}
		}, 
		/** right shift operator with sign extension */
		SHA {
			public String toString() {
				return ">>";
			}
		};
	}
}
