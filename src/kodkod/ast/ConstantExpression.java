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
	
	/**
	 * Constructs a constant expression with the given arity.
	 */
	ConstantExpression(String name, int arity) {
		super(name, arity);
	}

	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
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
    
}
