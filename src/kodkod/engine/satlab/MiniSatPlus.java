/**
 * MiniSatPlus.java
 * Created on 11:22:04 AM
 */
package kodkod.engine.satlab;

import java.util.Iterator;

import kodkod.engine.TimeoutException;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A wrapper that provides
 * access to the basic funcionality of the MiniSAT+ solver. 
 * 
 * @author jbaek
 */
public class MiniSatPlus implements SATSolverWithPB {

	// The memory address of MiniSATPlus loaded in this instance. 
	private final long minisat;
	private int timeout = Integer.MAX_VALUE;
	private int status;
	private static final int UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
				TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	
	/**
	 * Constructs a new wrapper for a freshly created
	 * instance of MiniSatPlus.
	 */
	MiniSatPlus() {
		this.minisat =  make();
		this.timeout = Integer.MAX_VALUE;
		//setTimeout(minisat, timeout); //TODO
	}
	
	/* 
	 * @see kodkod.engine.satlab.SATSolverWithPB#addPBClause(int[], int[], int, int)
	 */
	public void addPBClause(int[] lits, int[] cs, int rhs, int ineq) {
		if (lits == null || cs == null)
			throw new NullPointerException();
		else if (lits.length > 0 && lits.length == cs.length && ineq >= -2 && ineq <= 2) {
			addPBClause(minisat, lits, cs, rhs, ineq);
		}
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#addClause(int[])
	 */
	public void addClause(int[] lits) {
		// defer to addPBClause;
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
	 * Throws UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 * @see kodkod.engine.satlab.SATSolver#coreSize()
	 */
	public int coreSize() {
		throw new UnsupportedOperationException("This is not a core extracting solver.");
	}

	/**
	 * @see kodkod.engine.satlab.SATSolver#isCoreExtractor()
	 */
	public boolean isCoreExtractor() {
		return false;
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public int numberOfClauses() {
		return numConstraints(minisat);
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public int numberOfVariables() {
		return numVariables(minisat);
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#retainCore()
	 */
	public void retainCore() {
		throw new UnsupportedOperationException("This is not a core extracting solver.");
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#setTimeout(int)
	 */
	public void setTimeout(int seconds) {
		// TODO Auto-generated method stub
		// timeout not supported yet.
	}

	/* (non-Javadoc)
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

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#timeout()
	 */
	public int timeout() {
		return timeout;
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#unsatCore()
	 */
	public Iterator<int[]> unsatCore() {
		throw new UnsupportedOperationException("This is not a core extracting solver.");
	}

	/* (non-Javadoc)
	 * @see kodkod.engine.satlab.SATSolver#valueOf(int)
	 */
	public final boolean valueOf(int variable) {
		if (status != SATISFIABLE)
			throw new IllegalStateException();
		return valueOf(minisat, variable);
	}
	
	/* (non-Javadoc)
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
			if (valueOf(minisat, start)==(intTruth==1))
				ret.add(start);
		}
		return ret;
	}
	
	// native methods
	static {
		System.loadLibrary("miniSatJNI");
	}
	private static native long make();
	/**
	 * We'll skip addGoal for now, and just find a single solution.
	 */
	// omitted so far: getVar, allocConstants, solve(solve_Command?)
	//private static native void addGoal(long minisat, int[] lits, int[] cs); 
	
	// the ownership of the arrays lits and cs may be taken, I believe. Is that OK?
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
	
	// examples
	// make, free, setTimeout, numClause, solve, valueOf, coreSize, coreClause, retainCore
	private static native void addVariables(long minisat, int numVariables);
	private static native int numVariables(long minisat);
	private static native int numConstraints(long minisat);
	private static native void addClause(long minisat, int[] lits);
	private static native boolean valueOf(long minisat, int index);
}
