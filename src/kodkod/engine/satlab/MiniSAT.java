/**
 * 
 */
package kodkod.engine.satlab;


/**
 * Java wrapper for Niklas EŽn and Niklas Sšrensson MiniSAT solver.
 * @author Emina Torlak
 */
final class MiniSAT extends NativeSolver {
	
	/**
	 * Constructs a new MiniSAT wrapper.
	 */
	public MiniSAT() {
		super(make());
	}
	
	static {
		loadLibrary("minisat");
	}

	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "MiniSat";
	}
	
	/**
	 * Returns a pointer to an instance of  MiniSAT.
	 * @return a pointer to an instance of minisat.
	 */
	private static native long make();
	
	/**
	 * Releases the resources associated with
	 * the given instance of minisat.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of minisat
	 */
	native void free(long peer);
	
	/**
	 * Adds the given number of variables
	 * to the instance of minisat referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	native void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance
	 * of minisat referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(minisat) | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of minisat.
	 */
	native void addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * minisat referenced by the given long.
	 * @return true if sat, false otherwise
	 */
	native boolean solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of minisat:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(minisat)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	native boolean valueOf(long peer, int literal);
}
