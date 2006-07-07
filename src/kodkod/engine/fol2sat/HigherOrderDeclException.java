/**
 * 
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Decl;
import kodkod.ast.Multiplicity;

/**
 * Thrown when a node contains a higher order declaration that cannot
 * be skolemized, or it can be skolemized but skolemization is disabled.
 * 
 * @specfield decl: Decl // higher order decl that caused the exception to be thrown
 * @author Emina Torlak
 */
public final class HigherOrderDeclException extends RuntimeException {
	private final Decl decl;
	private static final long serialVersionUID = 1892780864484615171L;

	/**
	 * Constructs a HigherOrderDeclException for the given decl.
	 * @requires decl.multiplicity != ONE
	 * @effects this.decl' = decl
	 */
	 HigherOrderDeclException(Decl decl) {
		super("Higher order declaration: " + decl);
		assert decl.multiplicity() != Multiplicity.ONE;
		this.decl = decl;
	}

	/**
	 * Returns this.decl
	 * @return this.decl
	 */
	public Decl decl() {
		return decl;
	}

}
