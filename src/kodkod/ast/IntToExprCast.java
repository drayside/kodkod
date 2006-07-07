/**
 * 
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
	 * {@inheritDoc}
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() { 
		return intExpr.hashCode();
	}
	
	/**
	 * Returns true if o is an IntToExprCast whose intExpr is 
	 * logically equal to this.intExpr.
	 * @return o in IntToExprCast && o.intExpr.equals(this.intExpr)
	 */
	public boolean equals(Object o) {
		if (this==o) 
			return true;
		else if (o instanceof IntToExprCast) 
			return ((IntToExprCast)o).intExpr.equals(intExpr);
		else	 
			return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() { 
		return "((Expression)" + intExpr + ")";
	}
	
}
