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
	private static native void setCost(long zchaff, int variable, int cost);
	
	/**
	 * Retrieves the cost of the given variable in the native zchaff instance at the 
	 * given address.
	 * @requires variable is a valid variable identifier
	 * @return the cost of the given variable in the native zchaff instance at the 
	 * given address.
	 */
	private static native int costOf(long zchaff, int variable);
}
