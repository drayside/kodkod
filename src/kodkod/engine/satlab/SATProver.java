/**
 * 
 */
package kodkod.engine.satlab;

import java.util.Iterator;

/**
 * Provides an interface to a SAT solver that can generate
 * proofs of unsatisfiability.
 * 
 * @specfield literals: set int
 * @specfield clauses: set seq[literals]
 * @specfield unsatCore: set clauses
 * @invariant all i: [2..) | i in literals => i-1 in literals
 * @invariant !this.solve() => some unsatCore
 * @invariant all s: SATSolver | s.clauses = unsatCore => !s.solve()
 * @author Emina Torlak
 */
public interface SATProver extends SATSolver {

	/**
	 * Returns the size of the unsatisfiable core of this.clauses.
	 * @return #this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 */
	public abstract int coreSize();
	
	/**
	 * Returns an iterator over the unsatisifiable core 
	 * of this.clauses; i.e. a subset of this.clauses
	 * that makes the problem unsatisfiable.  The core
	 * is usually smaller than this.clauses.
	 * @return an Iterator over this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 */
	public abstract Iterator<int[]> unsatCore();
	
	/**
	 * Remove all clauses from this.clauses that are not
	 * in this.unsatCore.  The solve() method can be called
	 * after calling retainCore to obtain a smaller unsatisfiable core.
	 * Usually, one would repeat the retainCore/solve cycle until a 
	 * fixed point is reached; that is, until the value returned by
	 * the coreSize() method is no longer decreasing.
	 * @effects this.clauses' = this.unsatCore
	 * @throws IllegalStateException - {@link #solve() } has not been called or the 
	 * outcome of the last call was not <code>false</code>.
	 */
	public abstract void retainCore();
}
