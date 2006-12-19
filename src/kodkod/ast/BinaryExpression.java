/*
 * BinaryExpression.java
 * Created on Jun 30, 2005
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;


/** 
 * Represents a binary {@link kodkod.ast.Expression expression}.
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
	private final int hashCode;
	
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
		this.hashCode = op.hashCode() + left.hashCode() + right.hashCode();
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
	 * Returns true of o is a BinaryExpression with the
	 * same tree structure as this.
	 * @return o.op.equals(this.op) && o.left.equals(this.left) && o.right.equals(this.right) 
	 */
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof BinaryExpression)) return false;
		BinaryExpression that = (BinaryExpression)o;
		return op.equals(that.op) &&
		left.equals(that.left) &&
		right.equals(that.right);
	}
	
	public int hashCode() {
		return hashCode;
	}
	
	public String toString() {
		return "(" + left + " " + op + " " + right + ")";
	}
	
	/**
	 * A binary expression operator: union, difference, intersection, override, join, and product.
	 */
	public static enum Operator {
		
		DIFFERENCE { public String toString() { return "-"; }},
		UNION { public String toString() { return "+"; }},
		INTERSECTION { public String toString() { return "&"; }},
		OVERRIDE { public String toString() { return "++"; }},
		JOIN {
			public String toString() { return "."; }
			boolean applicable(int leftArity, int rightArity) { return leftArity + rightArity > 2; }
			int arity(int leftArity, int rightArity) { return leftArity + rightArity - 2; }
		},
		PRODUCT {
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
		
	}
}
