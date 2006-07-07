/**
 * 
 */
package kodkod.engine.fol2sat;

import kodkod.ast.LeafExpression;

/**
 * Thrown when a node contains an undeclared variable or a relation with no bounds.
 * @specfield leaf: LeafExpression // the unbound leaf that caused the exception to be thrown
 * @author Emina Torlak
 */
public final class UnboundLeafException extends RuntimeException {
	private final LeafExpression leaf;
	private static final long serialVersionUID = 2472395272061454465L;

	/**
	 * Constructs an UnboundLeafException using the given message and leaf.
	 * @effects this.leaf' = leaf
	 */
	UnboundLeafException(String msg, LeafExpression leaf) {
		super(msg);
		this.leaf = leaf;
	}

	/**
	 * Returns this.leaf.
	 * @return this.leaf
	 */
	public LeafExpression leaf() { 
		return leaf;
	}

}
