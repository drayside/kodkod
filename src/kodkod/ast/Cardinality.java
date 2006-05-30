/**
 * 
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * An {@link kodkod.ast.IntExpression } representing the 
 * cardinality of an {@link kodkod.ast.Expression}.
 * @specfield expression: Expression
 * @invariant children = expression
 * @author Emina Torlak
 */
public final class Cardinality extends IntExpression {
	private final Expression expression;
	
	/**  
	 * Constructs a new cardinality expression
	 * 
	 * @effects this.expression' = expression && this.op' = op
	 * @throws NullPointerException - expression = null || op = null
	 * @throws IllegalArgumentException - op = SUM && child.arity != 1
	 */
	Cardinality(Expression child) {
		this.expression = child;
	}
	
	/**
	 * Returns this.expression.
	 * @return this.expression
	 */
	public Expression expression() {return expression;}

	/**
	 * Returns true of o is a CardinaalityExpression with the
	 * same tree structure as this.
	 * @return o.expression.equals(this.expression) 
	 */
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Cardinality)) return false;
		Cardinality that = (Cardinality)o;
		return expression.equals(that.expression);
	}
	
	public int hashCode() {
		return expression.hashCode();
	}
	
	public String toString() {
		return expression.toString();
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
}
