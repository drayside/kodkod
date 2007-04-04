/**
 * 
 */
package kodkod.ast;



import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/** 
 * Represents a integer comparison formula, e.g. x = y, x <= y, etc.
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
	public static enum Operator {
		/** `=' operator */
		EQ { 
			public String toString() { return "="; }
			public boolean apply(int i0, int i1) { return i0==i1; }
		},
		/** `<' operator */
		LT { 
			public String toString() { return "<"; }
			public boolean apply(int i0, int i1) { return i0<i1; }
		},
		/** `<=' operator */
		LTE { 
			public String toString() { return "<="; }
			public boolean apply(int i0, int i1) { return i0<=i1; }
		},
		/** `>' operator */
		GT { 
			public String toString() { return ">"; }
			public boolean apply(int i0, int i1) { return i0>i1; }
		},
		/** `>=' operator */
		GTE { 
			public String toString() { return ">="; }
			public boolean apply(int i0, int i1) { return i0>=i1; }
		};

		/**
		 * Returns the result of comparing the given integers
		 * using this comparison operator.
		 * @return i0 op i1
		 */
		public abstract boolean apply(int i0, int i1);
	}
	
}
