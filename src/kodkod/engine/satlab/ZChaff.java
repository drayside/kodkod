/**
 * ZChaff.java
 * Created on 5:36:29 PM
 */
package kodkod.engine.satlab;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.engine.TimeoutException;
import kodkod.util.IntSet;
import kodkod.util.Ints;
import static kodkod.engine.satlab.ZChaffJNI.*;

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
	protected final long zchaff;
	private int timeout, status;
	
	/**
	 * Constructs a new wrapper for a freshly created
	 * instance of zchaff that provides the core
	 * extraction functionality if the enableCoreExtraction flag is set.
	 * Otherwise, the instance provides only the basic functionality.
	 * @effects this.zchaff' contains the memory address of a new
	 * instance of zchaff.
	 */
	ZChaff(boolean enableCoreExtraction) {
		this.zchaff = make(enableCoreExtraction);
		this.timeout = Integer.MAX_VALUE;
		ZChaffJNI.setTimeout(zchaff, timeout);
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
		ZChaffJNI.setTimeout(zchaff, timeout);
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
			ZChaffJNI.addVariables(zchaff, numVars);
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
			ZChaffJNI.addClause(zchaff, lits);
		}
	}

	/**
	 * Returns the current solver status.  It
	 * is an integer in the range [0..5], with 
	 * the meanings defined in ZChaffJNI.
	 * @return solver status
	 */
	protected final int status() {
		return status;
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
		status = ZChaffJNI.solve(zchaff);
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
		final int intTruth = truthValue ? ZChaffJNI.TRUE : ZChaffJNI.FALSE;
		for(; start <= end; start++) {
			if (valueOf(zchaff, start)==intTruth)
				ret.add(start);
		}
		return ret;
	}

	/**
	 * Releases the memory used by this.zchaff.
	 */
	protected void finalize() throws Throwable {
		super.finalize();
		free(zchaff);
//		System.out.println("finalizing " + zchaff);
	}
	
	/**
	 * A wrapper class that provides access to the basic
	 * functionality of the zchaff solver from princeton. 
	 * 
	 * @author Emina Torlak
	 */
	static final class Basic extends ZChaff {
		/**
		 * Constructs a new ZChaff.Basic solver.
		 */
		public Basic() {
			super(false);
		}
	}
	
	static final class Plus extends ZChaff implements CoreExtractor {
		/**
		 * Constructs a new ZChaff.Plus solver.
		 */
		public Plus() {
			super(true);
		}

		/**
		 * Throws an IllegalArgumentException if the solver 
		 * status is not UNSATISFIABLE.
		 * @throws IllegalStateException - status() != UNSATISFIABLE
		 */
		private void checkStatus() {
			if (status() != UNSATISFIABLE)
				throw new IllegalStateException();
		}
		
		/**
		 * Returns the size of the unsatisfiable core of this.clauses.
		 * @return #this.unsatCore
		 * @throws IllegalStateException - {@link #solve() } has not been called or the 
		 * outcome of the last call was not <code>false</code>.
		 * @see kodkod.engine.satlab.CoreExtractor#coreSize()
		 */
		public int coreSize() {
			checkStatus();
			return ZChaffJNI.coreSize(zchaff);
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
			checkStatus();
			return new Iterator<int[]>() {
				private int coreSize = coreSize(), cursor = 0;
				
				public boolean hasNext() {
					return cursor < coreSize;
				}

				public int[] next() {
					if (!hasNext()) throw new NoSuchElementException();
					return ZChaffJNI.coreClause(zchaff, cursor++);
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
			checkStatus();
			ZChaffJNI.retainCore(zchaff);
		}
	}
}
