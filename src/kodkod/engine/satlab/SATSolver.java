package kodkod.engine.satlab;

import kodkod.engine.TimeoutException;
import kodkod.util.ints.IntSet;

/**
 * Provides an interface to a SAT solver.
 * 
 * @specfield literals: set int
 * @specfield clauses: set seq[literals]
 * @invariant all i: [2..) | i in literals => i-1 in literals
 * @author Emina Torlak
 */
public interface SATSolver {
	/**
	 * Returns the maximum amount of time
	 * that this.solver will spend trying
	 * to find a solution. 
	 * @return the timeout (in seconds)
	 */
	public abstract int timeout();
	
	/**
	 * Returns the size of this solver's vocabulary.
	 * @return #this.literals
	 */
	public abstract int numberOfVariables();
	
	/**
	 * Returns the number of clauses added to the 
	 * solver so far.
	 * @return #this.clauses
	 */
	public abstract int numberOfClauses();
	
	/**
	 * Sets the timeout of this solver to the specified
	 * number of seconds.  If a solution is 
	 * not found in the given timeframe, the solver
	 * terminates with a TimeoutException.
	 * @requires seconds >= 0
	 * @effects sets the timeout to the given number of seconds
	 * @throws IllegalArgumentException - seconds < 0
	 */
	public abstract void setTimeout(int seconds);
	
	/**
	 * Adds the specified number of new variables
	 * to the solver's vocabulary.
	 * @requires numVars >= 0
	 * @effects this.literals' = [1..#this.literals + numVars]
	 * @throws IllegalArgumentException - numVars < 0
	 */
	public abstract void addVariables(int numVars);
	
	/**
	 * Adds the specified sequence of literals to this.clauses.
	 * No reference to the specified array is kept, so it can
	 * be reused.  <b>The contents of the array may, however, 
	 * be modified.</b>  It is the client's responsibility to 
	 * ensure that no literals in a clause are repeated, or that
	 * both a literal and its negation are present.
	 * @requires all i: [0..lits.length) | lits[i] != 0 && |lits[i]| <= #this.literals 
	 * @effects this.clauses' = this.clauses + lits
	 * @effects lits' may not have the same contents as lits
	 * @throws NullPointerException - lits = null
	 */
	public abstract void addClause(int[] lits);
	
	/**
	 * Returns true if there is a satisfying assignment for this.clauses.
	 * Otherwise returns false.  If this.clauses are satisfiable, the 
	 * satisfying assignment can be obtained by calling {@link #variablesThatAre(boolean, int, int)}.
	 * If the satisfiability of this.clauses cannot be determined within
	 * the given number of seconds, a TimeoutException is thrown.
	 * @return true if this.clauses are satisfiable; otherwise false.
	 * @throws TimeoutException - the solver could not determine
	 * the satisfiability of the problem within the specified number of seconds.
	 */
	public abstract boolean solve() throws TimeoutException;
	
	/**
	 * Returns the literals in the range [start..end] that 
	 * have been set to the given boolean value by the most recent call to {@link #solve() }.
	 * @requires {@link #solve() } has been called and the 
	 * outcome of the last call was <code>true</code>.  
	 * @return the true literals between start and end
	 * @throws IllegalArgumentException - start > end || [start..end] !in this.literals
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>true</code>.
	 */
	public abstract IntSet variablesThatAre(boolean truthValue, int start, int end);
}
