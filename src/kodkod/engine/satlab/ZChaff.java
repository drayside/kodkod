package kodkod.engine.satlab;

/**
 * Wrapper for an instance of zchaff that provides
 * access to basic functionality.
 */
final class ZChaff extends NativeSolver {
	/**
	 * Constructs an instance of ZChaffBasic.
	 */
	ZChaff() {
		super(make());
	}

	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "ZChaff";
	}

	static {
	    loadLibrary("zchaff");
	}
	
	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  
	 * @return the memory address of an instance
	 * of the zchaff solver 
	 */
	private static native long make();

	/**
	 * Releases the resources associated with
	 * the given instance of zchaff.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of zchaff
	 */
	native void free(long peer);
	

	/**
	 * Adds the given number of variables
	 * to the instance of zchaff referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	native void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance of zchaff referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(p) | i = l || i = -l
	 * @effects adds the given clause to the specified instance of zchaff.
	 * @return clause index of the given clause, if it was added to the peer;
	 * a negative integer if not
	 */
	native int addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * zchaff referenced by the given long.
	 * @return true if sat, false otherwise
	 */
	native boolean solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of zchaff:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(peer)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	native boolean valueOf(long peer, int literal);

}