/**
 * ZChaffBasic.java
 * Created on 5:36:29 PM
 */
package kodkod.engine.satlab;


import kodkod.engine.TimeoutException;

/**
 * A wrapper class that provides access to the 
 * zchaff solver from Princeton.
 * 
 * @author Emina Torlak
 */
abstract class ZChaff implements SATSolver {
	/**
	 * The memory address of the instance of zchaff
	 * wrapped by this wrapper.
	 */
	private long peer;
	private int status, clauses, vars;
	
	/**
	 * Constructs a new wrapper for the given 
	 * instance of zchaff.
	 */
	ZChaff(long peer) {
		this.peer = peer;
		this.clauses = this.vars = 0;
//		System.out.println("created " + peer);
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
	 * Returns the current status of the solver.
	 * @return one of UNDETERMINED = 0, UNSATISFIABLE=1, SATISFIABLE = 2,
	 *			 TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	 */
	final int status() { 
		return status;
	}
	
	/**
	 * Returns a pointer to the C++ peer class (the native instance
	 * of zchaff wrapped by this).
	 * @return a pointer to the C++ peer class (the native instance
	 * of zchaff wrapped by this).
	 */
	final long peer() {
		return peer;
	}
	
	/**
	 * Returns true if there is a satisfying assignment for this.clauses.
	 * Otherwise returns false.  If this.clauses are satisfiable, the 
	 * satisfying assignment can be obtained by calling {@link #variablesThatAre(boolean, int, int)}
	 * or {@link #valueOf(int) }.
	 * @return true if this.clauses are satisfiable; otherwise false.
	 * @throws TimeoutException - the solver could not determine
	 * the satisfiability of the problem within this.timeout() seconds.
	 * @see kodkod.engine.satlab.SATSolver#solve()
	 */
	public final boolean solve() throws TimeoutException {
		status = solve(peer);
		switch(status) {
		case UNDETERMINED : 
			throw new UnknownError();
		case UNSATISFIABLE:
			return false;
		case SATISFIABLE :
			return true;
		case TIME_OUT : 
			throw new TimeoutException();
		case MEM_OUT : 
			throw new OutOfMemoryError();
		case ABORTED : 
			throw new UnknownError();
		default :
			throw new InternalError();
		}
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
		if (status != SATISFIABLE)
			throw new IllegalStateException();
		validateVariable(variable);
		return valueOf(peer, variable)==1;
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
	
	
	/*----------------the native methods that call zchaff and the associated constants----------------*/
	static final int	 UNDETERMINED = 0, UNSATISFIABLE=1, SATISFIABLE = 2,
					 TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	
	/**
	 * Releases the resources associated with
	 * the given instance of zchaff.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of zchaff
	 */
	abstract void free(long peer);
	
	/**
	 * Adds the given number of variables
	 * to the instance of zchaff referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	abstract void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance
	 * of zchaff referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(zchaff) | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of zchaff.
	 */
	abstract void addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * zchaff referenced by the given long.
	 * @return an integer in the range [0..5], indicating the solver's
	 * status as follows: UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
	 * TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5.
	 */
	abstract int solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of zchaff:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(zchaff)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	abstract int valueOf(long peer, int literal);

	
}
