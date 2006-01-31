package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a constant valued expression.
 * 
 * @invariant no children
 * @author Emina Torlak
 */
public final class ConstantExpression extends LeafExpression {
	
	static final ConstantExpression UNIV = new ConstantExpression("univ", 1);
	
	static final ConstantExpression IDEN = new ConstantExpression("iden", 2);
	
	static final ConstantExpression NONE = new ConstantExpression("none", 1);
	
	/**
	 * Constructs a constant expression with the given arity.
	 */
	private ConstantExpression(String name, int arity) {
		super(name, arity);
	}

	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
	public <E, F, D> E accept(ReturnVisitor<E, F, D> visitor) {
		return visitor.visit(this);
	}
	
	/**
     * Accepts the given visitor.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
     */
    public void accept(VoidVisitor visitor) {
        visitor.visit(this);
    }
}
