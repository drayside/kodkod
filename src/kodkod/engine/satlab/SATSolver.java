package kodkod.engine.satlab;


/**
 * Provides an interface to a SAT solver.
 * 
 * @specfield variables: set [1..)
 * @specfield clauses: set Clause
 * @invariant all i: [2..) | i in variables => i-1 in variables
 * @invariant all c: clauses | all lit: c.literals | lit in variables || -lit in variables
 * @author Emina Torlak
 */
public interface SATSolver {
	
	/**
	 * Returns the size of this solver's vocabulary.
	 * @return #this.variables
	 */
	public abstract int numberOfVariables();
	
	/**
	 * Returns the number of clauses added to the 
	 * solver so far.
	 * @return #this.clauses
	 */
	public abstract int numberOfClauses();
	
	/**
	 * Adds the specified number of new variables
	 * to the solver's vocabulary.  The behavior of this 
	 * method is undefined if it is called after this.solve()
	 * has returned <tt>false</tt>.
	 * @requires numVars >= 0
	 * @effects this.variables' = [1..#this.variables + numVars]
	 * @throws IllegalArgumentException - numVars < 0
	 */
	public abstract void addVariables(int numVars);
	
	/**
	 * Adds a clause corresponding to 
	 * the specified literals to this.clauses.
	 * No reference to the specified array is kept, so it can
	 * be reused.  <b>The contents of the array may, however, 
	 * be modified.</b>  It is the client's responsibility to 
	 * ensure that no literals in the given array are repeated, or that
	 * both a literal and its negation are present.  The behavior of this 
	 * method is undefined if it is called after this.solve()
	 * has returned <tt>false</tt>.
	 * @requires all i: [0..lits.length) | lits[i] != 0 && |lits[i]| in this.variables 
	 * @effects #this.clauses' = #this.clauses + 1 and
	 *  let c = this.clauses' - this.clauses | no c.antecedents and c.literals = lits[int] and
	 *  no c.index & this.clauses.index
	 * @effects lits' may not have the same contents as lits
	 * @throws NullPointerException - lits = null
	 */
	public abstract void addClause(int[] lits);
	
	/**
	 * Returns true if there is a satisfying assignment for this.clauses.
	 * Otherwise returns false.  If this.clauses are satisfiable, the 
	 * satisfying assignment for a given variable
	 *  can be obtained by calling {@link #valueOf(int)}.
	 * If the satisfiability of this.clauses cannot be determined within
	 * the given number of seconds, a TimeoutException is thrown.
	 * @return true if this.clauses are satisfiable; otherwise false.
	 * @throws SATAbortedException -- the call to solve was cancelled or
	 * could not terminate normally.
	 */
	public abstract boolean solve() throws SATAbortedException;
	
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
	public abstract boolean valueOf(int variable);
	
	/**
	 * Frees the memory used by this solver.  Once free() is called,
	 * all subsequent calls to methods other than free() may fail.  
	 * @effects frees the memory used by this solver
	 */
	public abstract void free();
	
}
