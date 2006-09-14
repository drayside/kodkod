/**
 * 
 */
package kodkod.engine.satlab;



/**
 * A skeleton implementation of a wrapper for a sat solver
 * accessed through JNI.
 * 
 * @author Emina Torlak
 */
abstract class NativeSolver implements SATSolver {
	/**
	 * The memory address of the native instance wrapped by this wrapper.
	 */
	private long peer;
	private int clauses, vars;
	private Boolean status;
	/**
	 * Constructs a new wrapper for the given 
	 * instance of the native solver.
	 */
	NativeSolver(long peer) {
		this.peer = peer;
		this.clauses = this.vars = 0;
		this.status = null;
//		System.out.println("created " + peer);
	}
	
	/**
	 * Loads the JNI library with the given name.
	 */
	static void loadLibrary(String library) {
		
		try {
			System.loadLibrary(library);
		} catch(UnsatisfiedLinkError ule1) {
			
			final String fs = System.getProperty("file.separator");
			final String userdir = System.getProperty("user.dir") + fs;
			
			try {
				System.load(userdir+System.mapLibraryName(library));
			} catch(UnsatisfiedLinkError ule2) {
				
				String os = System.getProperty("os.name").toLowerCase().replace(' ','-');
				if (os.startsWith("mac-")) os="mac";
	             else if (os.startsWith("windows-")) os="windows";
				
				String arch = System.getProperty("os.arch").toLowerCase().replace(' ','-');
				if (arch.equals("powerpc")) arch="ppc";
				else arch = arch.replaceAll("\\Ai[3456]86\\z","x86");
				
				final String path = userdir + "jni" + fs + arch + "-" + os + fs;
				
				System.load(path+System.mapLibraryName(library));	
			}
			
		}
	}
	
	/**
	 * Returns the size of this solver's vocabulary.
	 * @return #this.labels
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public final int numberOfVariables() {
		return vars;
	}
	
	/**
	 * Returns the number of clauses added to the 
	 * solver so far.
	 * @return #this.clauses
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public final int numberOfClauses() {
		return clauses;
	}
	
	/**
	 * Adds the specified number of new variables
	 * to the solver's vocabulary.
	 * @effects this.labels' = [1..#this.literals + vars]
	 * @throws IllegalArgumentException - vars < 0
	 * @see kodkod.engine.satlab.SATSolver#addVariables(int)
	 */
	public final void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("vars < 0: " + numVars);
		else if (numVars > 0) {
			vars += numVars;
			addVariables(peer, numVars);
		}
	}
	
	/**
	 * Adds the specified sequence of literals to this.clauses.
	 * No reference to the specified array is kept, so it can
	 * be reused. 
	 * @effects this.clauses' = this.clauses + lits
	 * @throws NullPointerException - lits = null
	 * @throws IllegalArgumentException - some i: [0..lits.length) | |lits[0]| > #this.literals 
	 * @see kodkod.engine.satlab.SATSolver#addClause(int[])
	 */
	public final void addClause(int[] lits) {
		if (lits==null)
			throw new NullPointerException();
		else if (lits.length > 0) {
//			for(int i : lits) {
//				System.out.print(i + " ");
//			}
//			System.out.println(0);
			clauses++;
			addClause(peer, lits);
		}
	}
	
	/**
	 * Returns a pointer to the C++ peer class (the native instance
	 *  wrapped by this).
	 * @return a pointer to the C++ peer class (the native instance
	 *  wrapped by this).
	 */
	final long peer() {
		return peer;
	}
	
	/**
	 * Returns the current status of the solver.
	 * @return null if the status is unknown, TRUE if the last
	 * call to solve() yielded SAT, and FALSE if the last call to
	 * solve() yielded UNSAT.
	 */
	final Boolean status() { 
		return status;
	}
	
	/**
	 * Returns true if there is a satisfying assignment for this.clauses.
	 * Otherwise returns false.  If this.clauses are satisfiable, the 
	 * satisfying assignment can be obtained by calling {@link #variablesThatAre(boolean, int, int)}
	 * or {@link #valueOf(int) }.
	 * @return true if this.clauses are satisfiable; otherwise false.
	 * @see kodkod.engine.satlab.SATSolver#solve()
	 */
	public final boolean solve() {
		return (status = solve(peer));
	}
	
	
	
	/**
	 * Throws an IllegalArgumentException if variable !in this.variables.
	 * Otherwise does nothing.
	 * @throws IllegalArgumentException - variable !in this.variables
	 */
	final void validateVariable(int variable) {
		if (variable < 1 || variable > vars)
			throw new IllegalArgumentException(variable + " !in [1.." + vars+"]");
	}
	
	/**
	 * Returns the boolean value assigned to the given variable by the
	 * last successful call to {@link #solve()}. 
	 * @requires {@link #solve() } has been called and the 
	 * outcome of the last call was <code>true</code>.  
	 * @return the boolean value assigned to the given variable by the
	 * last successful call to {@link #solve()}. 
	 * @throws IllegalArgumentException - variable !in this.variables
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>true</code>.
	 */
	public final boolean valueOf(int variable) {
		if (!Boolean.TRUE.equals(status))
			throw new IllegalStateException();
		validateVariable(variable);
		return valueOf(peer, variable);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#free()
	 */
	public synchronized final void free() {
		if (peer!=0) {
//			System.out.println("freeing " + peer + " " + getClass());
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
	 * Releases the resources associated with
	 * the given instance of a native solver.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of a native solver
	 */
	abstract void free(long peer);
	
	/**
	 * Adds the given number of variables
	 * to the instance of a native solver referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	abstract void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance
	 * of a native solver referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..this.numVariables()] | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of a native solver.
	 */
	abstract void addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * a native solver referenced by the given long.
	 * @return true if the clauses in the solver are SAT;
	 * otherwise returns false.
	 */
	abstract boolean solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of a native solver
	 * @requires the last call to {@link #solve(long) solve(peer)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	abstract boolean valueOf(long peer, int literal);

}
