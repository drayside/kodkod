/**
 * ZChaff.java
 * Created on 1:43:34 PM
 */
package kodkod.engine.satlab;

import kodkod.engine.TimeoutException;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * The wrapper class for the zchaff solver from Princeton.
 * 
 * @author Emina Torlak
 */
public final class ZChaff implements SATSolver {
	private static final int UNDETERMINED = 0, UNSATISFIABLE = 1,
    SATISFIABLE = 2, TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	
	private long zchaff;
	private int status, timeout, vars, clauses;
	
	static {
	    System.load("/Users/emina/Documents/workspace3.1/relations/zchaff/libzchaffJNI.jnilib");
	}
	
	/**
	 * Constructs a wrapper for an instance of zchaff. 
	 */
	public ZChaff() {
		this.status = UNDETERMINED;
		this.timeout = Integer.MAX_VALUE;
		this.zchaff = initialize(timeout);
		this.vars = this.clauses = 0;
	}
	
	/**
	 * Creates an instance of zchaff with the given
	 * timeout, and returns its address in 
	 * memory.
	 */
	private native long initialize(int timeout);
	
	/**
	 * Adds the given number of variables
	 * to the instance of zchaff referenced
	 * by the first argument.
	 */
	private native void addVariables(long zchaff, int numVariables);
	
	/**
	 * Adds the specified claues to the instance
	 * of zchaff referenced by the first argument.
	 */
	private native void addClause(long zchaff, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * zchaff referenced by the given long.
	 */
	private native int solve(long zchaff);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of zchaff:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is undecided.
	 */
	private native int valueOf(long zchaff, int literal);
	
	/**
	 * Sets the timeout of the given instance of zchaff
	 * to the specified value.
	 */
	private native void setTimeout(long zchaff, int timeout);
	
	/**
	 * Releases the resources associated with
	 * the given instance of zchaff.
	 */
	private native void free(long zchaff);
	
	/**
	 * Returns the size of this solver's vocabulary.
	 * @return #this.literals
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public int numberOfVariables() {
		return vars;
	}

	/**
	 * Returns the number of clauses added to the 
	 * solver so far.
	 * @return #this.clauses
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public int numberOfClauses() {
		return clauses;
	}

	/**
	 * Returns the maximum amount of time
	 * that this.solver will spend trying
	 * to find a solution. 
	 * @return the timeout (in seconds)
	 * @see kodkod.engine.satlab.SATSolver#timeout()
	 */
	public int timeout() {
		return timeout;
	}

	/**
	 * Sets the timeout of this solver to the specified
	 * number of seconds.  If a solution is 
	 * not found in the given timeframe, the solver
	 * terminates with a TimeoutException.
	 * @effects sets the timeout to the given number of seconds
	 * @throws IllegalArgumentException - seconds < 0
	 * @see kodkod.engine.satlab.SATSolver#setTimeout(int)
	 */
	public void setTimeout(int seconds) {
		if (seconds < 0)
			throw new IllegalArgumentException(seconds + " < 0");
		timeout = seconds;
		setTimeout(zchaff, timeout);
	}

	/**
	 * Adds the specified number of new variables
	 * to the solver's vocabulary.
	 * @effects this.literals' = [1..#this.literals + vars]
	 * @throws IllegalArgumentException - vars < 0
	 * @see kodkod.engine.satlab.SATSolver#addVariables(int)
	 */
	public void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("vars < 0: " + numVars);
		else if (numVars > 0) {
			vars += numVars;
			addVariables(zchaff, vars);
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
	public void addClause(int[] lits) {
		if (lits==null)
			throw new NullPointerException();
		else if (lits.length > 0) {
			clauses++;
			// we must modify the lits to conform to the
			// zchaff input format:  2 * VarIndex + Sign. 
			// The Sign is 0 for positive phased literals,
			// and 1 for negative phased literals.
			// For example, a clause (3 -5 11 -4 ) should be represented by
			// { 6, 11, 22, 9 }
			for(int i = lits.length-1; i >= 0; i--) {
				int lit = lits[i];
				if (lit < 0)
					lits[i] = (-lit << 1) + 1;
				else 
					lits[i] = lit << 1; 
			}
			addClause(zchaff, lits);	
		}
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
	public boolean solve() throws TimeoutException {
		status = solve(zchaff);
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
	 * Returns the literals in the range [start..end] that 
	 * have been set to the given boolean value by the most recent call to {@link #solve() }.  
	 * @return the true literals between start and end
	 * @throws IllegalArgumentException - start > end || [start..end] !in this.literals
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>true</code>.
	 * @see kodkod.engine.satlab.SATSolver#variablesThatAre(boolean, int, int)
	 */
	public IntSet variablesThatAre(boolean truthValue, int start, int end) {
		if (status != SATISFIABLE)
			throw new IllegalStateException();
		if (start < 1 || start > end || end > numberOfVariables())
			throw new IllegalArgumentException("[start..end]: " + start + ".." + end + ", #this.literals: " + numberOfVariables());
		final IntSet ret = Ints.bestSet(start, end);
		final int intTruth = truthValue ? 1 : 0;
		for(; start <= end; start++) {
			if (valueOf(zchaff, start)==intTruth)
				ret.add(start);
		}
		return ret;
	}

	protected void finalize() throws Throwable {
		super.finalize();
		free(zchaff);
	}
	
	public static void main(String[] args) {
		final ZChaff z = new ZChaff();
		z.addVariables(3);
		int[] clause = {1,2,3};
		z.addClause(clause);
		int[] clause1 = {-3};
		z.addClause(clause1);
//		z.addVariables(2);
		
		try {
			System.out.println(z.solve());
			System.out.println(z.variablesThatAre(true, 1, 3));
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
