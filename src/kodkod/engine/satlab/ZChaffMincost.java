/**
 * 
 */
package kodkod.engine.satlab;

/**
 * Wrapper for an instance of MinCostZChaff.
 * 
 * @author Emina Torlak
 */
final class ZChaffMincost extends ZChaff implements SATMinSolver {

	/**
	 * Constructs an instance of ZChaffMincost.
	 */
	ZChaffMincost() {
		super(make());
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATMinSolver#setCost(int, int)
	 */
	public void setCost(int variable, int cost) {
		validateVariable(variable);
		if (cost < 0)
			throw new IllegalArgumentException("invalid cost: " + cost);
		setCost(peer(), variable, cost);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATMinSolver#costOf(int)
	 */
	public int costOf(int variable) {
		validateVariable(variable);
		return costOf(peer(), variable);
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() { 
		return "ZChaffMincost";
	}
	
	static {
		System.loadLibrary("zchaff_mincost");
	}

	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  
	 * @return the memory address of an instance
	 * of the zchaff solver 
	 */
	private static native long make();
	
	/**
	 * Sets the cost of the given variable to the specified value in the 
	 * native zchaff instance at the given address.
	 * @requires  variable is a valid variable identifier && cost >= 0
	 * @effects sets the cost of the given variable to the specified value in the 
	 * native zchaff instance at the given address.
	 */
	private native void setCost(long peer, int variable, int cost);
	
	/**
	 * Retrieves the cost of the given variable in the native zchaff instance at the 
	 * given address.
	 * @requires variable is a valid variable identifier
	 * @return the cost of the given variable in the native zchaff instance at the 
	 * given address.
	 */
	private native int costOf(long peer, int variable);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#free(long)
	 */
	native void free(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#addVariables(long, int)
	 */
	native void addVariables(long peer, int numVariables);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#addClause(long, int[])
	 */
	native void addClause(long peer, int[] lits);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#solve(long)
	 */
	native int solve(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#valueOf(long, int)
	 */
	native int valueOf(long peer, int literal);
	
}
