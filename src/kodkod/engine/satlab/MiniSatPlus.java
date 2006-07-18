/**
 * MiniSatPlus.java
 * Created on 11:22:04 AM
 */
package kodkod.engine.satlab;

import kodkod.engine.TimeoutException;

/**
 * A wrapper that provides
 * access to the basic funcionality of the MiniSat+ solver. 
 * 
 * @author jbaek
 */
public class MiniSatPlus implements SATSolverWithPB {

	// The memory address of MiniSat+ loaded in this instance. 
	private long minisat;
	private int status;
	private static final int UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
				TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	
	/**
	 * Creates a fresh instance of MiniSat+. 
	 */
	MiniSatPlus() {
		this.minisat =  make();
	}
	
	/**
	 * Adds the specified pseudo-boolean constraint to the existing instance of MiniSat+.
	 * @requires -2 <= ineq && ineq <=2 && lits.length == cs.length && lits.length > 0
	 * @throws NullPointerException - lits = null || cs = null
	 * @throws IllegalArgumentException - lits.length != cs.length || lits.length = 0 || ineq > 2 || ineq < -2
	 * @effects adds the given pseudo-boolean constraint
	 */
	public void addPBClause(int[] lits, int[] cs, int rhs, int ineq) {
		if (lits == null || cs == null)
			throw new NullPointerException();
		else if (lits.length > 0 && lits.length == cs.length && ineq >= -2 && ineq <= 2) {
			addPBClause(minisat, lits, cs, rhs, ineq);
		} else
			throw new IllegalArgumentException();
	}

	/**
	 * Trivially translates the given CNF clause into an equivalent pseudo-boolean
	 * constraint and passes it to MiniSat+ 
	 * of MiniSat+ referenced by the first argument.
	 * @requires -2 <= ineq && ineq <=2 
	 * @effects adds the given pseudo-boolean constraint to the specified instance of MiniSat+.
	 */
	public void addClause(int[] lits) {
		int rhs_coef = 0;
		if (lits == null)
			throw new NullPointerException();
		if (lits.length > 0) {
			int[] coeffs = new int[lits.length];
			for (int i = 0; i < coeffs.length; i++) {
				if (lits[i] == 0) throw new IllegalArgumentException();
				if (lits[i] > 0) coeffs[i] = 1; else
					{
						lits[i]*=-1;
						coeffs[i] = -1;
						rhs_coef--;
					}
			}
			addPBClause(lits, coeffs, rhs_coef, 2);
		}
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#addVariables(int)
	 */
	public void addVariables(int numVars) {
		// TODO Auto-generated method stub
		// note to self: I am not sure if miniSat requires configurations as these.

	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#free()
	 */
	public synchronized final void free() {
		if (minisat!=0) {
			// need to free minisat TODO
			minisat = 0;
		}
	}
	
	/**
	 * Returns the number of clauses added to the 
	 * solver so far.
	 * @return #this.clauses
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public int numberOfClauses() {
		return numConstraints(minisat);
	}

	/**
	 * Returns the size of this solver's vocabulary.
	 * @return #this.labels
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public int numberOfVariables() {
		return numVariables(minisat);
	}

	/**
	 * Unimplemented. MiniSat+ does not make use of timeout, so this
	 * method will perform no actions.
	 * @see kodkod.engine.satlab.SATSolver#setTimeout(int)
	 */
	public void setTimeout(int seconds) {
		return;
	}

	/**
	 * Returns true if there is a satisfying assignment for this.clauses.
	 * Otherwise returns false.  If this.clauses are satisfiable, the 
	 * satisfying assignment can be obtained by calling {@link #variablesThatAre(boolean, int, int)}
	 * or {@link #valueOf(int) }. Note that MiniSat+ has no timeout, so TimeoutException
	 * will actually be never thrown.
	 * @return true if this.clauses are satisfiable; otherwise false.
	 * @throws TimeoutException - the solver could not determine
	 * the satisfiability of the problem within this.timeout() seconds.
	 * @see kodkod.engine.satlab.SATSolver#solve()
	 */
	public boolean solve() throws TimeoutException {
		status = solve(minisat);
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
	 * Unimplemented. MiniSat+ does not make use of timeout, so this
	 * method will always return Integer.MAX_VALUE.
	 * @return Integer.MAX_VALUE
	 * @see kodkod.engine.satlab.SATSolver#timeout()
	 */
	public int timeout() {
		return Integer.MAX_VALUE;
	}

	/**
	 * Returns the boolean value assigned to the given variable by the
	 * last successful call to {@link #solve()}. 
	 * @requires {@link #solve() } has been called and the 
	 * outcome of the last call was <code>true</code>.  
	 * @return the boolean value assigned to the given variable by the
	 * last successful call to {@link #solve()}. 
	 * @throws IllegalArgumentException - variable cannot be validated.
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>true</code>.
	 */
	public final boolean valueOf(int variable) {
		if (status != SATISFIABLE)
			throw new IllegalStateException();
		if (!validateVariable(minisat, variable)) throw new IllegalArgumentException();
		return valueOf(minisat, variable);
	}
	
	/**
	 * Native methods begin here.
	 */
	
	static {
		System.loadLibrary("miniSatJNI");
	}
	
	/**
	 * Creates an instance of MiniSat+ and returns 
	 * its address in memory.  
	 * @return the memory address of an instance
	 * of the MiniSat+ solver 
	 */
	private static native long make();
	
	/**
	 * Adds the specified pseudo-boolean constraint to the instance
	 * of MiniSat+ referenced by the first argument.
	 * @requires -2 <= ineq && ineq <= 2 && lits.length == cs.length && lits.length > 0 
	 * @effects adds the given pseudo-boolean constraint to the specified instance of MiniSat+.
	 */
	private static native void addPBClause(long minisat, int[] lits, int[] cs, int rhs, int ineq);
	
	/**
	 * Calls the solve method on the instance of 
	 * minisat referenced by the given long.
	 * @return an integer in the range [0..5], indicating the solver's
	 * status as follows: UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
	 * TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5.
	 */
	private static native int solve(long minisat);
	//TODO: minisat's solve takes other options, i.e. return best solution, return all solutions, etc.
	//These functionalities are not taken advantage of, yet.
	
	/**
	 * Unimplemented
	 */
	private static native void addVariables(long minisat, int numVariables);
	
	/**
	 * Returns the number of existing variables in the given instance of MiniSat+.
	 * @return the number of existing variables.
	 */
	private static native int numVariables(long minisat);
	
	/**
	 * Returns the number of pseudo-boolean constraints in the given instance of MiniSat+.
	 * @return the number of pseudo-boolean constraints.
	 */
	private static native int numConstraints(long minisat);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of MiniSat+.
	 * @requires the last call to {@link #solve(long) solve(zchaff)} returned SATISFIABLE, and the variable was previously used in a pseudo-boolean constraint.
	 * @return the assignment for the given literal
	 */
	private static native boolean valueOf(long minisat, int index);
	
	/**
	 * Validates that the given literal specified by the second argument is currently assigned a value.
	 * @requires the last call to {@link #solve(long) solve(zchaff)} returned SATISFIABLE
	 * @return true if the given literal had been used and resolved, false otherwise
	 */
	private static native boolean validateVariable(long minisat, int index);
}
