/**
 * 
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a cast from an {@link kodkod.ast.IntExpression} to
 * an {@link kodkod.ast.Expression}.
 * @specfield intexpr: IntExpression
 * @invariant  children = intexpr
 * @invariant arity = 1
 * @author Emina Torlak
 */
public final class IntCastExpression extends Expression {
	private final IntExpression child;
	
	/**  
     * Constructs a new cast expression
     * @requires intexpr != null
     * @effects this.intexpr' = intexpr 
     */
	IntCastExpression(IntExpression intexpr) {
		this.child = intexpr;
	}

	/**
	 * Returns this.intexpr.
	 * @return this.intexpr
	 */
	public IntExpression intexpr() {
		return child;
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
	 * {@inheritDoc}
	 * @see kodkod.ast.Expression#apply(kodkod.ast.UnaryIntExpression.Operator)
	 */
	public IntExpression apply(UnaryIntExpression.Operator op) {
		return op==UnaryIntExpression.Operator.SUM ? child : super.apply(op);
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return child.hashCode();
	}
	
	/**
	 * Returns true if o is an IntCastExpression with the 
	 * same intexpr as this.
	 * @return o in IntCastExpression && this.intexpr.equals(o.intexpr)
	 */
	public boolean equals(Object o) {
		if (this==o) 
			return true;
		else if (o instanceof IntCastExpression) 
			return child.equals(((IntCastExpression)o).child);
		else 
			return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "((Expression)" + child + ")";
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

}
