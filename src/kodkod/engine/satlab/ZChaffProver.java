package kodkod.engine.satlab;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Wrapper for an instance of zchaff that providess access to
 * the core extracting functionality.
 * 
 * @author Emina Torlak
 */
final class ZChaffProver extends ZChaff implements SATProver {
	/**
	 * Constructs an instance of ZChaffProver.
	 */
	ZChaffProver() {
		super(make());
	}
	/**
	 * Returns the size of the unsatisfiable core of this.clauses.
	 * @return #this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 * @see kodkod.engine.satlab.CoreExtractor#coreSize()
	 */
	public int coreSize() {
		if (status() != ZChaff.UNSATISFIABLE)
			throw new IllegalStateException();
		return coreSize(peer());
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
		if (status() != ZChaff.UNSATISFIABLE)
			throw new IllegalStateException();
		return new Iterator<int[]>() {
			private int coreSize = coreSize(), cursor = 0;
			
			public boolean hasNext() {
				return cursor < coreSize;
			}
			
			public int[] next() {
				if (!hasNext()) throw new NoSuchElementException();
				return coreClause(peer(), cursor++);
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
		if (status() != ZChaff.UNSATISFIABLE)
			throw new IllegalStateException();
		retainCore(peer());
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() { 
		return "ZChaffProver";
	}
	
	static {
		System.loadLibrary("zchaff_prover");
	}
	
	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  
	 * @return the memory address of an instance
	 * of the zchaff solver 
	 */
	private static native long make();
	
	/**
	 * Returns the size of the unsatisfiable core produced by the
	 * given instance of zchaff.  
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE  
	 * @return the size of the unsat core produced by the given instance
	 * of zchaff.
	 */
	private native int coreSize(long peer);
	
	/**
	 * Returns the ith clause in the unsatisfiable core produced by the 
	 * given instance of zchaff. 
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE 
	 * @requires 0 <= i < {@link #coreSize(long) coreSize(zchaff) } 
	 * @return the ith clause in the unsatisfiable core of the given instance of zchaff
	 */
	private native int[] coreClause(long peer, int i);
	
	/**
	 * Removes all clauses from the given instance of zchaff that are not
	 * in the unsat core.
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE  
	 * @effects removes all clauses from the given instance of zchaff that are not
	 * in the unsat core.
	 */
	private native void retainCore(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#free(long)
	 */
	native void free(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#addVariables(long, int)
	 */
	native void addVariables(long peer, int numVariables);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#addClause(long, int[])
	 */
	native void addClause(long peer, int[] lits);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#solve(long)
	 */
	native int solve(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ZChaff#valueOf(long, int)
	 */
	native int valueOf(long peer, int literal);
}