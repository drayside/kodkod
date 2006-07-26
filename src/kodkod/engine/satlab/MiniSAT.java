/**
 * 
 */
package kodkod.engine.satlab;

import kodkod.engine.TimeoutException;

/**
 * Java wrapper for Niklas EŽn and Niklas Sšrensson MiniSAT solver.
 * @author Emina Torlak
 */
final class MiniSAT implements SATSolver {
	private int clauses, vars;
	/**
	 * The memory address of the instance of minisat
	 * wrapped by this wrapper.
	 */
	private long peer;
	
	/**
	 * Constructs a new MiniSAT wrapper.
	 */
	public MiniSAT() {
		super();
		peer = make();
		vars = clauses = 0;
	}
	
	static {
		System.loadLibrary("minisat");
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public int numberOfVariables() {
		return vars;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public int numberOfClauses() {
		return clauses;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#addVariables(int)
	 */
	public void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("vars < 0: " + numVars);
		else if (numVars > 0) {
			vars += numVars;
			addVariables(peer, numVars);
		}

	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#addClause(int[])
	 */
	public void addClause(int[] lits) {
		if (lits==null)
			throw new NullPointerException();
		else if (lits.length > 0) {
//			for(int i : lits) {
//				System.out.print(i + " ");
//			}
//			System.out.println(0);
			clauses ++;
			addClause(peer, lits);
		}
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#solve()
	 */
	public boolean solve() throws TimeoutException {
		return solve(peer);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#valueOf(int)
	 */
	public boolean valueOf(int variable) {
		if (variable < 1 || variable > vars)
			throw new IllegalArgumentException(variable + " !in [1.." + numberOfVariables()+"]");
		return valueOf(peer, variable);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#free()
	 */
	public synchronized void free() {
		if (peer!=0) {
//			System.out.println("freeing " + peer);
			free(peer);
			peer = 0;
		} // already freed

	}
	
	/**
	 * Releases the memory used by this.zchaff.
	 */
	protected final void finalize() throws Throwable {
		super.finalize();
		free();
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
	private static native void free(long peer);
	
	/**
	 * Adds the given number of variables
	 * to the instance of minisat referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	private static native void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance
	 * of minisat referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(minisat) | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of minisat.
	 */
	private static native void addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * minisat referenced by the given long.
	 * @return true if sat, false otherwise
	 */
	private static native boolean solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of minisat:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(minisat)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	private static native boolean valueOf(long peer, int literal);
}
