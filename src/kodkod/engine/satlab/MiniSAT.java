/**
 * MiniSAT.java
 * Created on 11:22:04 AM
 */
package kodkod.engine.satlab;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.engine.TimeoutException;
import kodkod.util.IntSet;
import kodkod.util.Ints;

import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.IVecInt;

/**
 * A wrapper class that provides
 * access to the basic funcionality of the MiniSAT solvers
 * (org.sat4j.specs.ISolver) from CRIL. 
 * 
 * @author Emina Torlak
 */
final class MiniSAT implements SATSolver {
	private final ISolver solver;
	private final ReadOnlyIVecInt wrapper;
	private Boolean isSatisfiable; 
	private int vars, clauses;
	
	/**
	 * Constructs a wrapper for the given instance
	 * of ISolver.
	 * @throws NullPointerException - solver = null
	 */
	MiniSAT(ISolver solver) {
		if (solver==null)
			throw new NullPointerException("solver");
		this.solver = solver;
		this.wrapper = new ReadOnlyIVecInt();
		this.isSatisfiable = null;
		this.vars = this.clauses = 0;
	}

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
		return solver.getTimeout();
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
			throw new IllegalArgumentException("seconds < 0: " + seconds);
		solver.setTimeout(seconds);
	}
	
	/**
	 * Adds the specified number of new variables
	 * to the solver's vocabulary.
	 * @effects this.literals' = [1..#this.literals + numVars]
	 * @throws IllegalArgumentException - numVars < 0
	 */
	public void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("numVars < 0: " + numVars);
		else if (numVars > 0) {
			vars += numVars;
			solver.newVar(vars);
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
		try {
			if (isSatisfiable != Boolean.FALSE) {
				clauses++;
				solver.addClause(wrapper.wrap(lits));
//				for(int lit : lits) {
//					System.out.print(lit + " ");
//				}
//				System.out.println(0);
			}
			
		} catch (ContradictionException e) {
			isSatisfiable = Boolean.FALSE;
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
	 */
	public boolean solve() throws TimeoutException {
		try {
			if (isSatisfiable != Boolean.FALSE)
				isSatisfiable = Boolean.valueOf(solver.isSatisfiable());
			return isSatisfiable;
		} catch (org.sat4j.specs.TimeoutException e) {
			throw new TimeoutException();
		} 
	}

	/**
	 * Returns the literals in the range [start..end] that 
	 * have been set to the given boolean value by the most recent call to {@link #solve() }.  
	 * @return the true literals between start and end
	 * @throws IllegalArgumentException - start > end || [start..end] !in this.literals
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>true</code>.
	 */
	public IntSet variablesThatAre(boolean truthValue, int start, int end) {
		if (isSatisfiable != Boolean.TRUE) 
			throw new IllegalStateException();
		if (start < 1 || start > end || end > solver.nVars())
			throw new IllegalArgumentException("[start..end]: " + start + ".." + end + ", #this.literals: " + solver.nVars());
		final IntSet ret = Ints.bestSet(start, end);
		final int switchValue = (truthValue ? 1 : -1);
		for(int lit : solver.model()) {
			lit *= switchValue;
			if (start <= lit && lit <= end)
				ret.add(lit);
			else if (lit > end || -lit > end)
				break;
		}
		return ret;
	}

	/**
	 * Returns false.
	 * @return false
	 */
	public boolean isCoreExtractor() {
		return false;
	}

	/**
	 * Throws UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public int coreSize() {
		throw new UnsupportedOperationException("This is not a core extracting solver.");
	}

	/**
	 * Throws UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public Iterator<int[]> unsatCore() {
		throw new UnsupportedOperationException("This is not a core extracting solver.");
	}

	/**
	 * Throws UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public void retainCore() {
		throw new UnsupportedOperationException("This is not a core extracting solver.");
	}
	
	/**
	 * A wrapper for an int array that provides
	 * read-only access to the array via the IVecInt interface. 
	 * 
	 * @author Emina Torlak
	 */
	private static final class ReadOnlyIVecInt implements IVecInt {
		private int[] vec;
		
		/**
		 * Sets this.vec to the given vector
		 * and returns this.
		 */
		IVecInt wrap(int[] vec) {
			this.vec = vec;
			return this;
		}
		
		public int size() {
			return vec.length;
		}

		public void shrink(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void shrinkTo(int arg0) {
			throw new UnsupportedOperationException();
		}

		public IVecInt pop() {
			throw new UnsupportedOperationException();
		}

		public void growTo(int arg0, int arg1) {
			throw new UnsupportedOperationException();
		}

		public void ensure(int arg0) {
			throw new UnsupportedOperationException();
		}

		public IVecInt push(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void unsafePush(int arg0) {
			throw new UnsupportedOperationException();
		}

		public int unsafeGet(int arg0) {
			return vec[arg0];
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public int last() {
			return vec[vec.length - 1];
		}

		public int get(int arg0) {
			if (arg0 < 0 || arg0 >= vec.length)
				throw new IndexOutOfBoundsException("arg0: " + arg0);
			return vec[arg0];
		}

		public void set(int arg0, int arg1) {
			throw new UnsupportedOperationException();		
		}

		public boolean contains(int arg0) {
			for(int i : vec) {
				if (i==arg0) return true;
			}
			return false;
		}

		public void copyTo(IVecInt arg0) {
			int argLength = arg0.size();
			arg0.ensure(argLength + vec.length);
			for(int i : vec) {
				arg0.set(argLength++, i);
			}
		}

		public void copyTo(int[] arg0) {
			assert arg0.length >= vec.length;
			System.arraycopy(vec,0, arg0, 0, vec.length);
		}

		public void moveTo(IVecInt arg0) {
			throw new UnsupportedOperationException();	
		}

		public void moveTo2(IVecInt arg0) {
			throw new UnsupportedOperationException();	
		}

		public void moveTo(int[] arg0) {
			throw new UnsupportedOperationException();	
		}

		public void moveTo(int arg0, int arg1) {
			throw new UnsupportedOperationException();	
		}

		public void insertFirst(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void remove(int arg0) {
			throw new UnsupportedOperationException();
		}

		public int delete(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void sort() {
			throw new UnsupportedOperationException();
		}

		public void sortUnique() {
			throw new UnsupportedOperationException();
		}

		public Iterator<Integer> iterator() {
			return new Iterator<Integer>() {
				int cursor = 0;
				public boolean hasNext() {
					return cursor < vec.length;
				}

				public Integer next() {
					if (!hasNext()) 
						throw new NoSuchElementException();
					return vec[cursor++];
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}
			};
		}
		
	}
	
	public static void main(String[] args) {
		final MiniSAT z = (MiniSAT)SATFactory.DefaultSAT4J.instance();
//		z.addVariables(3);
//		int[] clause = {1,2,3};
//		z.addClause(clause);
//		int[] clause1 = {-3};
//		z.addClause(clause1);
//		System.out.println(z.solver.nVars());
//		z.addVariables(4);
//		System.out.println(z.solver.nVars());
//		clause1[0] = 7;
//		z.addClause(clause1);
		z.addVariables(1);
		int[] clause1 = {1};
		z.addClause(clause1);
		clause1[0] = -1;
		z.addClause(clause1);
		try {
			System.out.println(z.solve());
			//System.out.println(z.variablesThatAre(true, 1, 1));
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	

}
