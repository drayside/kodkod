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
	native boolean solve(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#valueOf(long, int)
	 */
	native boolean valueOf(long peer, int literal);

}