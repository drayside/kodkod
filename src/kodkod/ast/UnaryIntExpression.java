/**
 * 
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a unary {@link kodkod.ast.IntExpression }, created by applying
 * the cardinality or sum operator  to an {@link kodkod.ast.Expression}.
 * @specfield expression: Expression
 * @specfield op: Operator
 * @invariant children = expression
 * @invariant op = SUM => expression.arity = 1
 * @author Emina Torlak
 */
public final class UnaryIntExpression extends IntExpression {
	private final Expression expression;
	private final Operator op;
	private final int hashCode;
	
	/**  
	 * Constructs a new unary int expression: op expression
	 * 
	 * @effects this.expression' = expression && this.op' = op
	 * @throws NullPointerException - expression = null || op = null
	 * @throws IllegalArgumentException - op = SUM && child.arity != 1
	 */
	UnaryIntExpression(Operator op, Expression child) {
		if (!op.applicable(child.arity())) {
			throw new IllegalArgumentException("Invalid arity: " + child + "::" + child.arity());
		}
		this.expression = child;
		this.op = op;
		this.hashCode = op.hashCode() + expression.hashCode();
	}
	
	/**
	 * Returns this.expression.
	 * @return this.expression
	 */
	public Expression expression() {return expression;}
	
	/**
	 * Returns this.op.
	 * @return this.op
	 */
	public Operator op() {return op;}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#toExpression()
	 */
	public Expression toExpression() {
		return op==Operator.SUM ? expression : super.toExpression();
	}
	
	/**
	 * Returns true of o is a UnaryIntExpression with the
	 * same tree structure as this.
	 * @return o.op.equals(this.op) && o.expression.equals(this.expression) 
	 */
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof UnaryIntExpression)) return false;
		UnaryIntExpression that = (UnaryIntExpression)o;
		return op.equals(that.op) &&
		expression.equals(that.expression);
	}
	
	public int hashCode() {
		return hashCode;
	}
	
	public String toString() {
		return op.toString() + expression.toString();
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
	 * Represents a unary operator for determining the integer value
	 * of a relational expression.
	 * 
	 * @author Emina Torlak
	 */
	public static enum Operator {
		CARDINALITY { 
			public String toString() { return "#"; }
			boolean applicable(int childArity) { return true; }
		},
		SUM { 
			public String toString() { return "$"; }
			boolean applicable(int childArity) { return childArity==1; }
		};
		/**
		 * @return true if this operator can be applied to an expression with the given arity.
		 */
		abstract boolean applicable(int childArity);
	}
	
}
