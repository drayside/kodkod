/**
 * ZChaff.java
 * Created on 5:36:29 PM
 */
package kodkod.engine.satlab;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.engine.TimeoutException;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * A wrapper class that provides access to the 
 * zchaff solver from Princeton.
 * 
 * @author Emina Torlak
 */
final class ZChaff implements SATSolver {
	/**
	 * The memory address of the instance of zchaff
	 * wrapped by this wrapper.
	 */
	private final long zchaff;
	private final boolean isCoreExtractor;
	private int timeout, status;
	
	/**
	 * Constructs a new wrapper for a freshly created
	 * instance of zchaff that provides the core
	 * extraction functionality if the enableCoreExtraction flag is set.
	 * Otherwise, the instance provides only the basic functionality.
	 */
	ZChaff(boolean enableCoreExtraction) {
		this.isCoreExtractor = enableCoreExtraction;
		this.zchaff = make(enableCoreExtraction);
		this.timeout = Integer.MAX_VALUE;
		setTimeout(zchaff, timeout);
	}
	
	/**
	 * Returns the size of this solver's vocabulary.
	 * @return #this.literals
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public final int numberOfVariables() {
		return numVariables(zchaff);
	}
	
	/**
	 * Returns the number of clauses added to the 
	 * solver so far.
	 * @return #this.clauses
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public final int numberOfClauses() {
		return numClauses(zchaff);
	}
	
	/**
	 * Returns the maximum amount of time
	 * that this.solver will spend trying
	 * to find a solution. 
	 * @return the timeout (in seconds)
	 * @see kodkod.engine.satlab.SATSolver#timeout()
	 */
	public final int timeout() {
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
	public final void setTimeout(int seconds) {
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
	public final void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("vars < 0: " + numVars);
		else if (numVars > 0) {
			addVariables(zchaff, numVars);
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
	public final boolean solve() throws TimeoutException {
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
	public final IntSet variablesThatAre(boolean truthValue, int start, int end) {
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
	
	/**
	 * Throws an IllegalArgumentException if the solver 
	 * status is not UNSATISFIABLE.  Throws an UnsupportedOperationException
	 * if !this.isCoreExtractor
	 * @throws IllegalStateException - this.status != UNSATISFIABLE
	 * @throws UnsupportedOperationException - !this.isCoreExtractor
	 */
	private void checkState() {
		if (status != UNSATISFIABLE)
			throw new IllegalStateException();
		if (!isCoreExtractor) 
			throw new UnsupportedOperationException("This is not a core extracting solver.");
	}
	
	/**
	 * Returns true if this instance of SATSolver can 
	 * extract an unsatisfiable core.
	 * @return true if this can extract unsat cores
	 */
	public boolean isCoreExtractor() {
		return isCoreExtractor;
	}
	
	/**
	 * Returns the size of the unsatisfiable core of this.clauses.
	 * @return #this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 * @see kodkod.engine.satlab.CoreExtractor#coreSize()
	 */
	public int coreSize() {
		checkState();
		return coreSize(zchaff);
	}
	
	/**
	 * Returns an iterator over the unsatisifiable core 
	 * of this.clauses; i.e. a subset of this.clauses
	 * that makes the problem unsatisfiable.  The core
	 * is usually smaller than this.clauses.
	 * @return an Iterator over this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 * @see kodkod.engine.satlab.CoreExtractor#unsatCore()
	 */
	public Iterator<int[]> unsatCore() {
		checkState();
		return new Iterator<int[]>() {
			private int coreSize = coreSize(), cursor = 0;
			
			public boolean hasNext() {
				return cursor < coreSize;
			}
			
			public int[] next() {
				if (!hasNext()) throw new NoSuchElementException();
				return coreClause(zchaff, cursor++);
			}
			
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}
	
	/**
	 * Remove all clauses from this.clauses that are not
	 * in this.unsatCore.
	 * @effects this.clauses' = this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 * @see kodkod.engine.satlab.CoreExtractor#retainCore()
	 */
	public void retainCore() {
		checkState();
		retainCore(zchaff);
	}
	
	/**
	 * Releases the memory used by this.zchaff.
	 */
	protected void finalize() throws Throwable {
		super.finalize();
		free(zchaff);
//		System.out.println("finalizing " + zchaff);
	}
	
	/*----------------the native methods that call zchaff and the associated constants----------------*/
	static {
	    System.loadLibrary("zchaffJNI");
	}
	
	private static final int	 UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
					 TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	
	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  The returned instance
	 * of zchaff will have its unsat core functionality
	 * enabled or disabled depending on the value of the
	 * enableCoreExtraction flag.
	 * @return the memory address of an instance
	 * of the zchaff solver that provides core extraction
	 * functionality if the enableCoreExtraction flag is true.
	 */
	private static native long make(boolean enableCoreExtraction);
	
	/**
	 * Releases the resources associated with
	 * the given instance of zchaff.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of zchaff
	 */
	private static native void free(long zchaff);
	
	/**
	 * Sets the timeout of the given instance of zchaff
	 * to the specified value.
	 * @effects sets the timeout to the given number of seconds
	 */
	private static native void setTimeout(long zchaff, int timeout);
	
	/**
	 * Adds the given number of variables
	 * to the instance of zchaff referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	private static native void addVariables(long zchaff, int numVariables);
	
	/**
	 * Returns the number of variables in the vocabulary of 
	 * the given instance of zchaff.
	 * @return the number of variables
	 */
	private static native int numVariables(long zchaff);
	
	/**
	 * Adds the specified clause to the instance
	 * of zchaff referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(zchaff) | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of zchaff.
	 */
	private static native void addClause(long zchaff, int[] lits);
	
	/**
	 * Returns the number of clauses in the pool of clauses
	 * of the given instance of zchaff.
	 * @return the number of clauses
	 */
	private static native int numClauses(long zchaff);
	
	/**
	 * Calls the solve method on the instance of 
	 * zchaff referenced by the given long.
	 * @return an integer in the range [0..5], indicating the solver's
	 * status as follows: UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
	 * TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5.
	 */
	private static native int solve(long zchaff);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of zchaff:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(zchaff)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	private static native int valueOf(long zchaff, int literal);

	/**
	 * Returns the size of the unsatisfiable core produced by the
	 * given instance of zchaff.  
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE  
	 * @return the size of the unsat core produced by the given instance
	 * of zchaff.
	 */
	private static native int coreSize(long zchaff);
	
	/**
	 * Returns the ith clause in the unsatisfiable core produced by the 
	 * given instance of zchaff. 
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE 
	 * @requires 0 <= i < {@link #coreSize(long) coreSize(zchaff) } 
	 * @return the ith clause in the unsatisfiable core of the given instance of zchaff
	 */
	private static native int[] coreClause(long zchaff, int i);
	
	/**
	 * Removes all clauses from the given instance of zchaff that are not
	 * in the unsat core.
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE  
	 * @effects removes all clauses from the given instance of zchaff that are not
	 * in the unsat core.
	 */
	private static native void retainCore(long zchaff);
	
	private static int[] pack(int... lits) {
		return lits;
	}
	
	public static void main(String[] args) {
		final long z = make(true);
		addVariables(z, 5);
		addClause(z, pack(1));
		addClause(z, pack(2));
		addClause(z, pack(3,4));
		addClause(z, pack(1, -5));
		addClause(z, pack(2, -5));
		addClause(z, pack(-1,-2,5));
		addClause(z, pack(-5));
		System.out.println(numVariables(z) + " " + numClauses(z));
		System.out.println(solve(z));
		System.out.println(coreSize(z));
		for(int i = coreSize(z)-1; i >= 0; i--) {
			System.out.println(Arrays.toString(coreClause(z,i)));
		}
		System.out.println(numClauses(z));
		retainCore(z);
		System.out.println("retained core...");
		System.out.println(numVariables(z) + " " + numClauses(z));
		System.out.println(solve(z));
		System.out.println(coreSize(z));
		for(int i = coreSize(z)-1; i >= 0; i--) {
			System.out.println(Arrays.toString(coreClause(z,i)));
		}
	}
	
}
