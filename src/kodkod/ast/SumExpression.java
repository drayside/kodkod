/**
 * 
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Denotes he integer obtained by summing the values of an iteger expression <i>ie</i>
 * for all values of a scalar <i>x</i> drawn from a set <i>e</i>.
 * @specfield intExpr: IntExpression
 * @specfield decls: Decls
 * @specfield children = intExpr + decls
 * @invariant  all d: decls.children | d.multiplicity = 1
 * @author Emina Torlak
 */
public final class SumExpression extends IntExpression {
	private final Decls decls;
	private final IntExpression intExpr;

	/**
	 * Constructs a sum expression
	 * @effects this.decls' = decls && this.intExpr' = intExpr
	 * @throws IllegalArgumentException - some d: decls.children | d.multiplicty != ONE
	 */
	SumExpression(Decls decls, IntExpression intExpr) {
		for(Decl d : decls) {
			if (d.multiplicity()!=Multiplicity.ONE)
				throw new IllegalArgumentException(d + " is not a scalar declaration.");
		}
		this.decls = decls;
		this.intExpr = intExpr;
	}

	/**
	 * Returns this.decls.
	 * @return this.decls
	 */
	public final Decls declarations() { 
		return decls;
	}
	
	/**
	 * Returns this.intExpr.
	 * @return this.intExpr
	 */
	public final IntExpression intExpr() { 
		return intExpr;
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
	 * Returns the string representation of this int expression.
	 * @return string representation of this int expression
	 */
	public String toString() { 
		return "(sum " + decls + " | " + intExpr + ")";
	}
	
	
}
